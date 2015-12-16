-- posts table
DROP TABLE IF EXISTS guestbook.posts;
CREATE TABLE guestbook.posts (
    id STRING PRIMARY KEY,
    user OBJECT(STRICT) AS (
        name STRING,
        location GEO_POINT
    ),
    text STRING INDEX USING FULLTEXT WITH (analyzer = 'english'),
    created TIMESTAMP,
    image_ref STRING,
    like_count LONG
) WITH (number_of_replicas = '0-2');

-- countries table
DROP TABLE IF EXISTS guestbook.countries;
CREATE TABLE guestbook.countries (
    id STRING PRIMARY KEY,
    name STRING PRIMARY KEY,
    geometry GEO_SHAPE
) WITH (number_of_replicas='0-2');

-- images blob table
DROP BLOB TABLE IF EXISTS guestbook_images;
CREATE BLOB TABLE guestbook_images
WITH (number_of_replicas='0-2');
