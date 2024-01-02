DROP TABLE IF EXISTS game_session;

CREATE TABLE game_session (
 id bigint NOT NULL AUTO_INCREMENT,
  sum_player_hand bigint NOT NULL,
  dealer_card varchar(10) NOT NULL,
  move varchar(10) NOT NULL,
  PRIMARY KEY (id)
);