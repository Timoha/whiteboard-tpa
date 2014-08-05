CREATE TABLE drawing
( drawing_id bigserial PRIMARY KEY
, board_id bigint NOT NULL REFERENCES board ON DELETE CASCADE
, strokes json
, first_name text NOT NULL
, last_name text NOT NULL
, email text NOT NULL
, created timestamp NOT NULL
, submitted timestamp
);


CREATE TYPE paper_type AS ENUM ('ANSI-A', 'ANSI-B', 'ANSI-C', 'ANSI-D', 'ANSI-E', 'ISO-A4', 'ISO-A3', 'ISO-A2', 'ISO-A1', 'ISO-A0');

CREATE TABLE board
( board_id bigserial PRIMARY KEY
, instance_id text NOT NULL
, name text NOT NULL
, width int NOT NULL
, height int NOT NULL
, paper_size paper_type NOT NULL
, background_color text NOT NULL
, background_picture text
, locked boolean DEFAULT FALSE NOT NULL
, created timestamp NOT NULL
);


CREATE TABLE widget_settings
( instance_id text NOT NULL
, component_id text NOT NULL
, board_id bigint REFERENCES board ON DELETE RESTRICT
, settings json --panel color, icon color, secondary color, border width, border color
, PRIMARY KEY (instance_id, component_id)
);
