DROP TABLE IF EXISTS nba.boxscores;

CREATE TABLE nba.boxscores
(
    season_id character varying(255) COLLATE pg_catalog."default",
    player_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    player_name character varying(255) COLLATE pg_catalog."default",
    team_id character varying(255) COLLATE pg_catalog."default",
    team_abbreviation character varying(255) COLLATE pg_catalog."default",
    team_name character varying(255) COLLATE pg_catalog."default",
    game_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    game_date date,
    matchup character varying(255) COLLATE pg_catalog."default",
    wl character varying COLLATE pg_catalog."default",
    min integer,
    fgm integer,
    fga integer,
    fgthreem integer,
    fgthreea integer,
    ftm integer,
    fta integer,
    oreb integer,
    dreb integer,
    reb integer,
    ast integer,
    stl integer,
    blk integer,
    tov integer,
    pf integer,
    pts integer,
    plus_minus numeric,
    video_available integer,
    home_away character varying(255) COLLATE pg_catalog."default",
    season integer,
    season_label character varying(255) COLLATE pg_catalog."default",
    season_type character varying(255) COLLATE pg_catalog."default",
    CONSTRAINT boxscores_pkey PRIMARY KEY (player_id, game_id)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE nba.boxscores
    OWNER to postgres;