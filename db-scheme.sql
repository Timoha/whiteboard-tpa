CREATE TABLE drawing
( drawing_id bigserial PRIMARY KEY
, board_id bigint NOT NULL REFERENCES board ON DELETE CASCADE
, strokes json
, first_name text NOT NULL
, last_name text NOT NULL
, email text NOT NULL
, created timestamp with timezone NOT NULL
, submitted timestamp with timezone
);


CREATE TABLE board
( board_id bigserial PRIMARY KEY
, instance_id text NOT NULL
, name text NOT NULL
, paper_size text NOT NULL
, background_color json NOT NULL
, background_picture text
, locked boolean DEFAULT FALSE NOT NULL
, created timestamp with timezone NOT NULL
);


CREATE TABLE widget_settings
( instance_id text NOT NULL
, component_id text NOT NULL
, board_id bigint REFERENCES board ON DELETE RESTRICT
, settings json -- border width
, PRIMARY KEY (instance_id, component_id)
);
