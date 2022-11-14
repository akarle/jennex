-- Parties just group a series of guests so they can RSVP for eachother
CREATE TABLE parties (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  notes TEXT
);
  
-- Guests store individual preferences for meals, etc
CREATE TABLE guests (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name VARCHAR(255) NOT NULL,
  party_id INTEGER,
  going INTEGER,
  meal_choice VARCHAR(255),
  has_plus1 INTEGER NOT NULL,
  plus1_going INTEGER,
  plus1_meal_choice VARCHAR(255),
  
  FOREIGN KEY(party_id) REFERENCES parties(id)
);

INSERT INTO parties (id) VALUES (1), (2);

INSERT INTO guests (name, party_id, has_plus1) VALUES
  ('Alex', 1, False),
  ('Jennie', 1, False),
  ('Matt', 2, True),
  ('Sarah', NULL, True),
  ('Sammy', 2, True);
