package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/mitchellh/mapstructure"
	"github.com/segmentio/ksuid"
	"io/ioutil"
	"net/http"
	"reflect"
	"time"
)

type Http struct {
	cfg *Config
}

func (h *Http) Init(cfg *Config) error {
	h.cfg = cfg

	return nil
}

func (h Http) Destroy() {}

// function for refreshing the data table
func (h Http) refreshTable(tableName string) error {
	_, err := h.processRequest(fmt.Sprintf("REFRESH TABLE %s", tableName))

	if err != nil {
		return fmt.Errorf("can't refresh the table: %s", tableName)
	}

	return nil
}

// processRequest is processing http request
func (h Http) processRequest(sql string, args ...interface{}) (*CrateResponse, error) {

	if len(sql) == 0 {
		return nil, errors.New("sql cannot be empty")
	}

	url := fmt.Sprintf("http://%s:%s@%s:%d/_sql", h.cfg.Crate.Username, h.cfg.Crate.Password, h.cfg.Crate.Host, h.cfg.Crate.HttpPort)

	// should be used the following json for passing data to crate db
	req := struct {
		SQL  string        `json:"stmt"`
		Args []interface{} `json:"args,omitempty"`
	}{
		SQL:  sql,
		Args: args,
	}
	// marshalling struct to json
	jb, err := json.Marshal(req)
	if err != nil {
		return nil, err
	}

	// preparing request
	request, err := http.NewRequest(http.MethodPost, url, bytes.NewBuffer(jb))
	if err != nil {
		return nil, err
	}

	request.Header.Set("Content-Type", "application/json")

	// processing request
	client := &http.Client{}
	response, err := client.Do(request)
	if err != nil {
		return nil, err
	}

	defer response.Body.Close()

	// getting body response
	b, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return nil, err
	}

	// error happened
	if response.StatusCode < http.StatusOK || response.StatusCode > http.StatusIMUsed {
		errResp := struct {
			Error struct {
				Message string `json:"message"`
				Code    int    `json:"code"`
			} `json:"error"`
		}{}

		if err := json.Unmarshal(b, &errResp); err != nil {
			return nil, err
		}

		return nil, fmt.Errorf("error during the sql processing: %s", errResp.Error.Message)
	}

	// sql was processed successfully
	// parsing response
	resp := CrateResponse{}
	if err := json.Unmarshal(b, &resp); err != nil {
		return nil, err
	}

	return &resp, nil
}

// function for getting of post details
func (h Http) getPostDetails(id string) ([]*PostDetails, error) {

	resp, err := h.processRequest(`SELECT h.*, c.name as country, c.geometry as area
		            	FROM guestbook.posts AS h, guestbook.countries AS c
		            	WHERE within(h."user"['location'], c.geometry) AND h.id = ?`, id)
	if err != nil {
		return nil, err
	} else if resp.RowCount == 0 {
		return nil, errors.New("no records were found")
	}

	result, err := processResponse[PostDetails](resp)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (h Http) SearchPosts(text string) (int, []*PostDetails, error) {

	response, err := h.processRequest(
		`SELECT h.*, h._score as _score,
			  c.name as country, c.geometry as area
			FROM guestbook.posts AS h, guestbook.countries AS c
			WHERE within(h."user"['location'], c.geometry)
			  AND match(h.text, $1)`,
		text)
	if err != nil {
		return 0, nil, err
	}

	result, err := processResponse[PostDetails](response)
	if err != nil {
		return 0, nil, err
	}

	return http.StatusOK, result, nil
}

func (h Http) GetAllPosts() (int, []*PostDetails, error) {

	// processing http request to crate db
	resp, err := h.processRequest(
		`SELECT h.*, c.name as country, c.geometry as area
			FROM guestbook.posts AS h, guestbook.countries AS c
			WHERE within(h."user"['location'], c.geometry)
			ORDER BY h.created DESC`)
	if err != nil {
		return http.StatusInternalServerError, nil, err
	} else if resp.RowCount == 0 {
		return http.StatusInternalServerError, nil, errors.New("no records were found")
	}

	// parsing received data
	result, err := processResponse[PostDetails](resp)
	if err != nil {
		return http.StatusInternalServerError, nil, err
	}

	return http.StatusOK, result, nil
}

func (h Http) GetPostById(id string) (int, []*PostDetails, error) {
	details, err := h.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve post details: %s", err)
	}

	return http.StatusOK, details, nil
}

func (h Http) CreateNewPost(req *NewPostRequest) (int, []*PostDetails, error) {

	id := ksuid.New().String()

	_, err := h.processRequest(
		`INSERT INTO guestbook.posts (id, "user", text, created, image_ref, like_count)
			VALUES ($1, $2, $3, $4, $5, $6)`,
		id,
		req.User,
		req.Text,
		time.Now().Format("2006-01-02 15:04:05"),
		req.ImageRef,
		0)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't update posts table: %s", err)
	}

	if err := h.refreshTable("guestbook.posts"); err != nil {
		return http.StatusInternalServerError, nil, err
	}

	result, err := h.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve added post: %s", err)
	}

	return http.StatusCreated, result, nil
}

func (h Http) UpdatePost(id string, req *UpdatePostRequest) (int, []*PostDetails, error) {

	_, err := h.processRequest(
		`UPDATE guestbook.posts SET text=$2 WHERE id=$1`,
		id,
		req.Text,
	)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't update posts table: %s", err)
	}

	if err := h.refreshTable("guestbook.posts"); err != nil {
		return http.StatusInternalServerError, nil, err
	}

	details, err := h.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve added post: %s", err)
	}

	return http.StatusOK, details, nil
}

func (h Http) DeletePost(id string) (int, error) {

	com, err := h.processRequest(
		`DELETE FROM guestbook.posts WHERE id=$1`,
		id,
	)
	if err != nil {
		return http.StatusInternalServerError, fmt.Errorf("can't delete posts table: %s", err)
	}

	if com.RowCount == 0 {
		return http.StatusNotFound, fmt.Errorf(`post with id=%s" not found`, id)

	}

	return http.StatusNoContent, nil
}

func (h Http) LikePost(id string) (int, *PostDetails, error) {

	com, err := h.processRequest(
		`UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=$1`,
		id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf(fmt.Sprintf("Can't increase likes count: %s", err))
	}

	if com.RowCount == 0 {
		return http.StatusNotFound, nil, fmt.Errorf(`post with id=%s" not found`, id)
	}

	details, err := h.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve post: %s", err)
	}

	return http.StatusOK, details[0], nil
}

func (h Http) GetAllImages() (int, []*AllImagesResponse, error) {

	response, err := h.processRequest(`SELECT digest, last_modified FROM blob.guestbook_images`)
	if err != nil {
		return http.StatusInternalServerError, nil, err
	}

	result, err := processResponse[AllImagesResponse](response)
	if err != nil {
		return 0, nil, err
	}

	return http.StatusOK, result, nil
}

func processResponse[T any](resp *CrateResponse) ([]*T, error) {

	result := []*T{}

	// hook for converting of timestamps to time.Time
	toTimeHookFunc := func() mapstructure.DecodeHookFunc {
		return func(
			f reflect.Type,
			t reflect.Type,
			data interface{}) (interface{}, error) {
			if t != reflect.TypeOf(time.Time{}) {
				return data, nil
			}

			switch f.Kind() {
			case reflect.String:
				return time.Parse(time.RFC3339, data.(string))
			case reflect.Float64:
				return time.Unix(0, int64(data.(float64))*int64(time.Millisecond)), nil
			case reflect.Int64:
				return time.Unix(0, data.(int64)*int64(time.Millisecond)), nil
			default:
				return data, nil
			}
		}
	}

	for _, row := range resp.Rows {
		if row == nil {
			continue
		}

		// preparing map
		m := make(map[string]interface{})
		for i, col := range resp.Cols {
			m[col] = row[i]
		}

		newItem := new(T)

		// converting map to struct
		decoder, err := mapstructure.NewDecoder(&mapstructure.DecoderConfig{
			WeaklyTypedInput: true,
			Result:           newItem,
			TagName:          "json",
			DecodeHook:       mapstructure.ComposeDecodeHookFunc(toTimeHookFunc()),
		})
		if err != nil {
			return nil, err
		}

		if err := decoder.Decode(m); err != nil {
			return nil, err
		}

		result = append(result, newItem)

	}

	return result, nil
}

type CrateResponse struct {
	Cols     []string        `json:"cols,omitempty"`
	Rows     [][]interface{} `json:"rows"`
	RowCount int             `json:"rowcount"`
	Duration float64         `json:"duration"`
}
