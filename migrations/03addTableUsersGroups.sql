CREATE TABLE usersgroups ( user_id BIGINT NOT NULL REFERENCES users(user_id), group_id BIGINT NOT NULL REFERENCES groups(group_id));


