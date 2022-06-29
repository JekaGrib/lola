CREATE TABLE groups ( group_id BIGSERIAL PRIMARY KEY NOT NULL, group_name VARCHAR(55) NOT NULL, group_pic_id BIGINT NOT NULL REFERENCES pics(pic_id), group_info VARCHAR(1005) NOT NULL);


