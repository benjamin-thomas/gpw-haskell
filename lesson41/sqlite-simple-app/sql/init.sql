/*
 
 Initialize or reset with:
 
 sqlite3 ./tools.db <./sql/init.sql
 
 NOTE: INT != INTEGER in sqlite!! (int autoincrement won't work)
 
 */
DROP TABLE IF EXISTS checked_out;
DROP TABLE IF EXISTS tools;
DROP TABLE IF EXISTS users;
CREATE TABLE users (
    user_id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL
);
CREATE TABLE tools (
    tool_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    descr TEXT NULL,
    last_returned TEXT NULL,
    times_borrowed INTEGER DEFAULT 0
);
CREATE TABLE checked_out (
    user_id INTEGER NOT NULL REFERENCES users (user_id),
    tool_id INTEGER UNIQUE NOT NULL REFERENCES tools (tool_id)
);
-- -- Sample data
INSERT INTO users (username)
VALUES ('will-kurt'),
    ('john-doe');
INSERT INTO tools (name, descr)
VALUES ('hammer', 'A powerful hammer!'),
    ('saw', 'An circular electric saw');