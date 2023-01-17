package main

import (
	"bytes"
	"crypto/sha1"
	"encoding/base64"
	"encoding/hex"
	"flag"
	"fmt"
	"github.com/gin-gonic/gin"
	yaml "gopkg.in/yaml.v2"
	"io/ioutil"
	"net/http"
	"os"
	"strconv"
)

func main() {

	// getting config
	cfg, err := readConfig("app.yaml")
	if err != nil {
		printError("unable to read config file", err)
	}

	fHttp := flag.Bool("http", false, "HTTP endpoint")
	fPq := flag.Bool("postgresql", false, "PostgreSQL wire protocol")
	flag.Parse()

	var provider Provider

	if *fHttp {
		fmt.Println("HTTP endpoint enabled")
		provider = new(Http)
	} else if *fPq {
		fmt.Println("PostgreSQL wire protocol enabled")
		provider = new(PostgreSQL)
    } else {
        fmt.Fprintln(os.Stderr, "ERROR: No provider selected")
        flag.Usage()
        os.Exit(1)
    }

	if err := provider.Init(cfg); err != nil {
		printError("can't initialize provider", err)
	}

	defer provider.Destroy()

	// creating server
	server := gin.Default()

	// setting CORS
	server.Use(func() gin.HandlerFunc {
		return func(c *gin.Context) {
			c.Writer.Header().Set("Access-Control-Allow-Origin", "*")
			c.Writer.Header().Set("Access-Control-Request-Headers", "*")
			c.Writer.Header().Set("Access-Control-Allow-Credentials", "true")
			c.Writer.Header().Set("Access-Control-Allow-Headers", "Content-Type, Content-Length, Accept-Encoding, X-CSRF-Token, Authorization, accept, origin, Cache-Control, X-Requested-With")
			c.Writer.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS, GET, PUT, DELETE")

			if c.Request.Method == "OPTIONS" {
				c.AbortWithStatus(204)
				return
			}

			c.Next()
		}
	}())

	server.GET("/", func(c *gin.Context) {
		c.String(http.StatusOK, "server is ready")
	})

	// searching
	server.POST("/search", func(c *gin.Context) {
		req := struct {
			QueryString string `json:"query_string"`
		}{}

		if err := c.ShouldBind(&req); err != nil {
			writeErrorResponse(c, http.StatusBadRequest, "Query_string is required")
			return
		}

		status, posts, err := provider.SearchPosts(req.QueryString)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, posts)
	})

	// getting all posts
	server.GET("/posts", func(c *gin.Context) {

		status, posts, err := provider.GetAllPosts()
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, posts)
	})

	// getting post by id
	server.GET("/post/:id", func(c *gin.Context) {

		id := c.Param("id")
		if len(id) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Post id has incorrect format")
			return
		}

		status, post, err := provider.GetPostById(id)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, post)
	})

	// adding a new post
	server.POST("/posts", func(c *gin.Context) {
		req := NewPostRequest{}

		if err := c.ShouldBind(&req); err != nil {
			writeErrorResponse(c, http.StatusBadRequest, "Can't parse post data")
			return
		}

		if req.User == nil {
			writeErrorResponse(c, http.StatusBadRequest, "user is required")
			return
		} else if len(req.User.Name) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "user should have a name")
			return
		} else if req.User.Location[0] == 0 || req.User.Location[1] == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "location is required")
			return
		}

		status, post, err := provider.CreateNewPost(&req)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, post)
	})

	// updating an existing post
	server.PUT("/post/:id", func(c *gin.Context) {

		id := c.Param("id")
		if len(id) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Record id is required")
			return
		}
		req := UpdatePostRequest{}

		if err := c.ShouldBind(&req); err != nil {
			writeErrorResponse(c, http.StatusBadRequest, "Can't parse post data")
			return
		}

		if len(req.Text) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "New post text is required")
			return
		}
		status, post, err := provider.UpdatePost(id, &req)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, post)
	})

	// deleting an existing post
	server.DELETE("/post/:id", func(c *gin.Context) {

		id := c.Param("id")
		if len(id) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Record id is required")
			return
		}

		status, err := provider.DeletePost(id)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.String(status, "")
	})

	// increments the number of likes for a given post.
	server.PUT("/post/:id/like", func(c *gin.Context) {

		id := c.Param("id")
		if len(id) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Record id is required")
			return
		}

		status, post, err := provider.LikePost(id)
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, post)
	})

	// getting all images that are saved in the crate blob store.
	server.GET("/images", func(c *gin.Context) {

		status, images, err := provider.GetAllImages()
		if err != nil {
			writeErrorResponse(c, status, err.Error())
			return
		}

		c.JSON(status, images)
	})

	// uploading an image
	server.POST("/images", func(c *gin.Context) {
		req := struct {
			Blob string `json:"blob"`
		}{}

		if err := c.ShouldBind(&req); err != nil {
			writeErrorResponse(c, http.StatusBadRequest, "Blob parameter is required")
			return
		}

		if len(req.Blob) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Blob parameter is required")
			return
		}

		b, err := base64.StdEncoding.DecodeString(req.Blob)
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't decode blob: %s", err))
			return
		}

		d := sha1.Sum(b)
		digest := hex.EncodeToString(d[:])

		// sending blob to crate
		url := fmt.Sprintf(`%sguestbook_images/%s`, cfg.Blob.BlobURL, digest)
		request, err := http.NewRequest(http.MethodPut, url, bytes.NewBuffer(b))
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't create an http request: %s", err))
			return
		}

		client := &http.Client{}
		resp, err := client.Do(request)
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't receive a response from crate: %s", err))
			return
		}

		defer resp.Body.Close()

		if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusConflict {
			writeErrorResponse(c, http.StatusInternalServerError, "Can't receive a response from crate")
			return
		}

		response := struct {
			Url    string `json:"url"`
			Digest string `json:"digest"`
		}{
			Url:    fmt.Sprintf(`/image/%s`, digest),
			Digest: digest,
		}

		c.JSON(resp.StatusCode, response)
	})

	// getting the image for a given digest
	server.GET("/image/:digest", func(c *gin.Context) {
		digest := c.Param("digest")
		if len(digest) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Digest is required")
			return
		}

		url := fmt.Sprintf(`%sguestbook_images/%s`, cfg.Blob.BlobURL, digest)
		request, err := http.NewRequest(http.MethodGet, url, bytes.NewBuffer(nil))
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't create an http request: %s", err))
			return
		}

		client := &http.Client{}
		resp, err := client.Do(request)
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't receive a response from crate: %s", err))
			return
		}

		defer resp.Body.Close()

		if resp.StatusCode != http.StatusOK {
			writeErrorResponse(c, http.StatusNotFound, fmt.Sprintf(`Image with digest="%s" not found`, digest))
			return
		}

		b, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't read response: %s", err))
			return
		}

		c.Header("Content-Length", strconv.Itoa(len(b)))
		c.Data(http.StatusOK, "image/gif", b)
	})

	// deleting an image from a blob storage
	server.DELETE("/image/:digest", func(c *gin.Context) {
		digest := c.Param("digest")
		if len(digest) == 0 {
			writeErrorResponse(c, http.StatusBadRequest, "Digest is required")
			return
		}

		url := fmt.Sprintf(`%sguestbook_images/%s`, cfg.Blob.BlobURL, digest)
		request, err := http.NewRequest(http.MethodDelete, url, bytes.NewBuffer(nil))
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't create an http request: %s", err))
			return
		}

		client := &http.Client{}
		resp, err := client.Do(request)
		if err != nil {
			writeErrorResponse(c, http.StatusInternalServerError, fmt.Sprintf("Can't receive a response from crate: %s", err))
			return
		}

		defer resp.Body.Close()

		if resp.StatusCode != http.StatusNoContent {
			writeErrorResponse(c, http.StatusNotFound, fmt.Sprintf(`Image with digest="%s" not found`, digest))
			return
		}

		c.String(http.StatusNoContent, "")
	})

	// running server
	if err := server.Run(fmt.Sprintf("%s:%s", cfg.Server.Host, cfg.Server.Port)); err != nil {
		printError("can't create server", err)
	}

}

// readConfig is reading config settings from yaml file
func readConfig(filename string) (*Config, error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}

	defer func() { _ = f.Close() }()

	decoder := yaml.NewDecoder(f)

	cfg := Config{}

	if err := decoder.Decode(&cfg); err != nil {
		return nil, err
	}

	return &cfg, nil
}

// printError is printing error to console and abort application
func printError(msg string, err error) {
	fmt.Fprintf(os.Stderr, "%s: %v\n", msg, err)
	os.Exit(1)
}

func writeErrorResponse(c *gin.Context, status int, errMessage string) {
	c.JSON(status, ErrorResponse{
		Status: status,
		Error:  errMessage,
	})
}

type Config struct {
	Server struct {
		Host string `yaml:"host"`
		Port string `yaml:"port"`
	}
	Crate struct {
		Host     string `yaml:"host"`
		HttpPort uint16 `yaml:"http_port"`
		PgPort   uint16 `yaml:"pg_port"`
		Username string `yaml:"username"`
		Password string `yaml:"password"`
	}
	Blob struct {
		BlobURL  string `yaml:"blob_url"`
		BlobMime string `yaml:"blob_mime"`
	}
}

type ErrorResponse struct {
	Status int    `json:"status"`
	Error  string `json:"error"`
}
