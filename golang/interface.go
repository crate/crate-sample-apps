package main

import "time"

type Provider interface {
	Init(cfg *Config) error
	SearchPosts(text string) (int, []*PostDetails, error)
	GetAllPosts() (int, []*PostDetails, error)
	GetPostById(id string) (int, *PostDetails, error)
	CreateNewPost(n *NewPostRequest) (int, []*PostDetails, error)
	UpdatePost(id string, u *UpdatePostRequest) (int, *PostDetails, error)
	DeletePost(id string) (int, error)
	LikePost(id string) (int, *PostDetails, error)
	GetAllImages() (int, []*AllImagesResponse, error)
	Destroy()
}

type NewPostRequest struct {
	User *struct {
		Name     string     `json:"name"`
		Location [2]float64 `json:"location"`
	} `json:"user"`
	Text     string  `json:"text"`
	ImageRef *string `json:"image_ref,omitempty"`
}

type UpdatePostRequest struct {
	Text string `json:"text"`
}

type AllImagesResponse struct {
	Digest       string    `json:"digest"`
	LastModified time.Time `json:"last_modified"`
}

type UploadImageRequest struct {
	Blob string `json:"blob"`
}

type ImageDetails struct {
	Url    string `json:"url"`
	Digest string `json:"digest"`
}

type PostDetails struct {
	ID   string `json:"id"`
	User *struct {
		Name     string    `json:"name"`
		Location []float64 `json:"location"`
	} `json:"user"`
	Text      string      `json:"text"`
	Created   time.Time   `json:"created"`
	ImageRef  *string     `json:"image_ref"`
	LikeCount int64       `json:"like_count"`
	Country   string      `json:"country"`
	Area      interface{} `json:"area"`
	Score     *float64    `json:"_score,omitempty" db:"_score"`
}
