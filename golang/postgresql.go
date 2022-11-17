package main

import (
	"context"
	"errors"
	"fmt"
	"github.com/georgysavva/scany/v2/pgxscan"
	"github.com/jackc/pgx/v5/pgxpool"
	"github.com/segmentio/ksuid"
	"net/http"
	"time"
)

type PostgreSQL struct {
	cfg     *Config
	conPool *pgxpool.Pool
}

func (p *PostgreSQL) Init(cfg *Config) error {
	// connecting to DB
	conPool, err := pgxpool.New(context.Background(), fmt.Sprintf("postgres://%s:%s@%s:%d", cfg.Crate.Username, cfg.Crate.Password, cfg.Crate.Host, cfg.Crate.PgPort))
	if err != nil {
		return err
	}

	p.conPool = conPool
	p.cfg = cfg

	return nil
}

func (p PostgreSQL) Destroy() {
	p.conPool.Close()
}

// function for refreshing the data table
func (p PostgreSQL) refreshTable(tableName string) error {
	_, err := p.conPool.Exec(context.Background(), fmt.Sprintf("REFRESH TABLE %s", tableName))

	if err != nil {
		return fmt.Errorf("can't refresh the table: %s", tableName)
	}

	return nil
}

// function for getting of post details
func (p PostgreSQL) getPostDetails(id string) ([]*PostDetails, error) {
	result := []*PostDetails{}

	err := pgxscan.Select(context.Background(), p.conPool, &result,
		`SELECT p.*, c.name as country, c.geometry as area
		            	FROM guestbook.posts AS p, guestbook.countries AS c
		            	WHERE within(p."user"['location'], c.geometry) AND p.id = $1`, id)

	if err != nil {
		return nil, err
	} else if len(result) == 0 {
		return nil, errors.New("no records were returned")
	}

	return result, nil
}

func (p PostgreSQL) SearchPosts(text string) (int, []*PostDetails, error) {
	result := []*PostDetails{}

	err := pgxscan.Select(context.Background(), p.conPool, &result,
		`SELECT p.*, p._score as _score,
		              c.name as country, c.geometry as area
		            FROM guestbook.posts AS p, guestbook.countries AS c
		            WHERE within(p."user"['location'], c.geometry)
		              AND match(p.text, $1)`, text)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("error during the search processing: %s", err)
	}

	return http.StatusOK, result, nil
}

func (p PostgreSQL) GetAllPosts() (int, []*PostDetails, error) {
	result := []*PostDetails{}

	err := pgxscan.Select(context.Background(), p.conPool, &result,
		`SELECT p.*, c.name as country, c.geometry as area
		            FROM guestbook.posts AS p, guestbook.countries AS c
		            WHERE within(p."user"['location'], c.geometry)
		            ORDER BY p.created DESC`)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve all posts from db: %s", err)
	}

	return http.StatusOK, result, nil
}

func (p PostgreSQL) GetPostById(id string) (int, []*PostDetails, error) {
	details, err := p.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve post details: %s", err)
	}

	return http.StatusOK, details, nil
}

func (p PostgreSQL) CreateNewPost(req *NewPostRequest) (int, []*PostDetails, error) {
	id := ksuid.New().String()
	_, err := p.conPool.Exec(context.Background(),
		`INSERT INTO guestbook.posts (id, "user", text, created, image_ref, like_count)
			VALUES ($1, $2, $3, $4, $5, $6)`,
		id,
		req.User,
		req.Text,
		time.Now().Format("2006-01-02 15:04:05"),
		req.ImageRef,
		0,
	)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't update posts table: %s", err)
	}

	if err := p.refreshTable("guestbook.posts"); err != nil {
		return http.StatusInternalServerError, nil, err
	}

	details, err := p.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve added post: %s", err)
	}

	return http.StatusCreated, details, nil

}

func (p PostgreSQL) UpdatePost(id string, req *UpdatePostRequest) (int, []*PostDetails, error) {
	_, err := p.conPool.Exec(context.Background(),
		`UPDATE guestbook.posts SET text=$2 WHERE id=$1`,
		id,
		req.Text,
	)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't update posts table: %s", err)
	}

	if err := p.refreshTable("guestbook.posts"); err != nil {
		return http.StatusInternalServerError, nil, err
	}

	details, err := p.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve added post: %s", err)
	}

	return http.StatusOK, details, nil
}

func (p PostgreSQL) DeletePost(id string) (int, error) {
	com, err := p.conPool.Exec(context.Background(),
		`DELETE FROM guestbook.posts WHERE id=$1`,
		id,
	)
	if err != nil {
		return http.StatusInternalServerError, fmt.Errorf("can't delete posts table: %s", err)
	}

	if com.RowsAffected() == 0 {
		return http.StatusNotFound, fmt.Errorf(`post with id=%s" not found`, id)
	}

	return http.StatusNoContent, nil
}

func (p PostgreSQL) LikePost(id string) (int, *PostDetails, error) {
	com, err := p.conPool.Exec(context.Background(),
		`UPDATE guestbook.posts SET like_count = like_count + 1 WHERE id=$1`,
		id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't increase likes count: %s", err)
	}

	if com.RowsAffected() == 0 {
		return http.StatusNotFound, nil, fmt.Errorf(`post with id=%s" not found`, id)
	}

	details, err := p.getPostDetails(id)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve post: %s", err)
	}

	return http.StatusOK, details[0], nil

}

func (p PostgreSQL) GetAllImages() (int, []*AllImagesResponse, error) {
	result := []*AllImagesResponse{}

	err := pgxscan.Select(context.Background(), p.conPool, &result,
		`SELECT digest, last_modified FROM blob.guestbook_images`)
	if err != nil {
		return http.StatusInternalServerError, nil, fmt.Errorf("can't retrieve images: %s", err)
	}

	return http.StatusOK, result, nil
}
