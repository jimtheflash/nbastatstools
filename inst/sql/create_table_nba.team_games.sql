DROP TABLE IF EXISTS nba.team_games;

CREATE TABLE nba.team_games
(
    team_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    team_abbreviation character varying(255) COLLATE pg_catalog."default",
    team_name character varying(255) COLLATE pg_catalog."default",
    game_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    matchup character varying(255) COLLATE pg_catalog."default",
    game_date date,
    season_id character varying(255) COLLATE pg_catalog."default",
    season integer,
    season_label character varying(255) COLLATE pg_catalog."default",
    home_away character varying(255) COLLATE pg_catalog."default",
    min_team integer,
    fgm_team integer,
    fga_team integer,
    fgthreem_team integer,
    fgthreea_team integer,
    ftm_team integer,
    fta_team integer,
    oreb_team integer,
    dreb_team integer,
    reb_team integer,
    ast_team integer,
    stl_team integer,
    blk_team integer,
    tov_team integer,
    pf_team integer,
    pts_team integer,
    players_used integer,
    team_season_game_num integer,
    is_backtoback_front integer,
    is_backtoback_back integer,
    is_backtoback integer,
    opponent_team_id character varying COLLATE pg_catalog."default",
    fgm_opp integer,
    fga_opp integer,
    fgthreem_opp integer,
    fgthreea_opp integer,
    ftm_opp integer,
    fta_opp integer,
    oreb_opp integer,
    dreb_opp integer,
    reb_opp integer,
    ast_opp integer,
    stl_opp integer,
    blk_opp integer,
    tov_opp integer,
    pf_opp integer,
    pts_opp integer,
    players_used_opp integer,
    season_game_num_opp integer,
    is_backtoback_front_opp integer,
    is_backtoback_back_opp integer,
    is_backtoback_opp integer,
    wl character varying COLLATE pg_catalog."default",
    margin numeric,
    CONSTRAINT team_games_pkey PRIMARY KEY (team_id, game_id)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE nba.team_games
    OWNER to postgres;