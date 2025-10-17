-- Create a database schema with 3 tables: users, posts, and comments

-- Table 1: Users
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    email TEXT
);

-- Table 2: Posts
CREATE TABLE posts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    title TEXT NOT NULL,
    content TEXT,
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Table 3: Comments
CREATE TABLE comments (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    post_id INTEGER,
    comment_text TEXT NOT NULL,
    FOREIGN KEY (post_id) REFERENCES posts(id)
);

-- Table 3: Login
CREATE TABLE login (
    user_id,
    login_time TEXT NOT NULL,
    info TEXT,
    PRIMARY KEY (user_id, login_time)
    FOREIGN KEY (user_id) REFERENCES users(id)
);

-- Insert users with normal and special characters
INSERT INTO users (username, email) VALUES
('alice', 'alice@example.com'),
('bob', 'bob@example.com'),
('charlie', 'charlie''s_email@example.com'); -- Escaped single quote in email

-- Insert posts with text values containing quotes and semicolons
INSERT INTO posts (user_id, title, content) VALUES
(1, 'Hello World', 'This is Alice''s first post! -- It''s exciting.'),
(2, 'A day in "Quotes"', 'Bob said: "It''s a /*great day*/; let''s make the most of it!"'),
(3, 'Charlie''s Adventures', 'Semicolons are fun; they /* separate ideas; like this.');

-- Insert comments with mixed text
INSERT INTO comments (post_id, comment_text) VALUES
(1, 'Nice post, Alice!'),
(2, 'I love the use of "quotation marks" in your title.'),
(3, 'Haha; I laughed at the semicolon usage. Keep it up!');
