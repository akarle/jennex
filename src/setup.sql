-- Start fresh
DROP TABLE parties;
DROP TABLE guests;

-- Parties just group a series of guests so they can RSVP for eachother
CREATE TABLE parties (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  notes TEXT
);
  
-- Guests store individual preferences for meals, etc
CREATE TABLE guests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name VARCHAR(255) NOT NULL,
  party_id INTEGER NOT NULL,
  going INTEGER,
  meal_choice VARCHAR(255),
  
  FOREIGN KEY(party_id) REFERENCES parties(id)
);

INSERT INTO parties (id) VALUES (1), (2), (3);

INSERT INTO guests (name, party_id) VALUES
  ('Alex', 1),
  ('Jennie', 1),
  ('Matt', 2),
  ('Sarah', 3),
  ('Sarah''s plus 1', 3),
  ('Sammy', 2);
