CREATE TABLE pics ( pic_id BIGSERIAL PRIMARY KEY NOT NULL, pic BYTEA NOT NULL);

CREATE TABLE key ( create_admin_key VARCHAR(55) NOT NULL);

CREATE TABLE users ( user_id BIGSERIAL PRIMARY KEY NOT NULL, password VARCHAR(55) NOT NULL, first_name VARCHAR(55) NOT NULL, last_name  VARCHAR(55) NOT NULL, user_pic_id BIGINT NOT NULL REFERENCES pics(pic_id), user_create_date DATE NOT NULL, admin boolean NOT NULL, token_key VARCHAR(55) NOT NULL );


CREATE TABLE authors (author_id BIGSERIAL PRIMARY KEY NOT NULL, author_info VARCHAR(505) NOT NULL, user_id BIGINT NOT NULL REFERENCES users(user_id), UNIQUE (user_id) );

CREATE TABLE tags ( tag_id BIGSERIAL PRIMARY KEY NOT NULL, tag_name VARCHAR(55) NOT NULL);

CREATE TABLE categories ( category_id BIGSERIAL PRIMARY KEY NOT NULL, category_name VARCHAR(55) NOT NULL, super_category_id BIGINT);

CREATE TABLE posts ( post_id BIGSERIAL PRIMARY KEY NOT NULL, author_id BIGINT NOT NULL REFERENCES authors(author_id), post_name VARCHAR(55) NOT NULL, post_create_date DATE NOT NULL, post_category_id BIGINT NOT NULL REFERENCES categories(category_id), post_text VARCHAR(10005) NOT NULL, post_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id));

CREATE TABLE comments ( comment_id BIGSERIAL PRIMARY KEY NOT NULL, comment_text VARCHAR(505) NOT NULL, post_id BIGINT NOT NULL REFERENCES posts(post_id), user_id BIGINT NOT NULL REFERENCES users(user_id));

CREATE TABLE postspics ( post_id BIGINT NOT NULL REFERENCES posts(post_id), pic_id BIGINT NOT NULL REFERENCES pics(pic_id));

CREATE TABLE poststags ( post_id BIGINT NOT NULL REFERENCES posts(post_id), tag_id BIGINT NOT NULL REFERENCES tags(tag_id));

CREATE TABLE drafts ( draft_id BIGSERIAL PRIMARY KEY NOT NULL, post_id BIGINT REFERENCES posts(post_id), author_id BIGINT NOT NULL REFERENCES authors(author_id), draft_name VARCHAR(55) NOT NULL, draft_category_id BIGINT NOT NULL REFERENCES categories(category_id), draft_text VARCHAR(10005) NOT NULL, draft_main_pic_id BIGINT NOT NULL REFERENCES pics(pic_id));

CREATE TABLE draftspics ( draft_id BIGINT NOT NULL REFERENCES drafts(draft_id), pic_id BIGINT NOT NULL REFERENCES pics(pic_id));

CREATE TABLE draftstags ( draft_id BIGINT NOT NULL REFERENCES drafts(draft_id), tag_id BIGINT NOT NULL REFERENCES tags(tag_id));


