-- This script is idempodent, meaning it can safely be run repeatedly.
-- It's a nice tradeoff for small projects: maintain a slightly more verbose
-- schema file in exchange for not having to deal with migrations. We also have
-- a harder-than-normal time renaming stuff, but that's OK for now.

CREATE TABLE IF NOT EXISTS rooms (id SERIAL PRIMARY KEY);
ALTER TABLE rooms ADD COLUMN IF NOT EXISTS created_at TIMESTAMP NOT NULL DEFAULT NOW();
-- Daily.co columns <https://docs.daily.co/reference#rooms>
ALTER TABLE rooms ADD COLUMN IF NOT EXISTS url TEXT NOT NULL;
ALTER TABLE rooms ADD COLUMN IF NOT EXISTS name TEXT NOT NULL;

CREATE TABLE IF NOT EXISTS stats (id SERIAL PRIMARY KEY);
ALTER TABLE stats ADD COLUMN IF NOT EXISTS room_id INTEGER NOT NULL REFERENCES rooms(id);
ALTER TABLE stats ADD COLUMN IF NOT EXISTS user_id TEXT NOT NULL;
ALTER TABLE stats ADD COLUMN IF NOT EXISTS recorded_at TIMESTAMP NOT NULL;
ALTER TABLE stats ADD COLUMN IF NOT EXISTS video_recv_bits_per_second INTEGER NOT NULL;
ALTER TABLE stats ADD COLUMN IF NOT EXISTS video_recv_packet_loss INTEGER NOT NULL;
ALTER TABLE stats ADD COLUMN IF NOT EXISTS video_send_bits_per_second INTEGER NOT NULL;
ALTER TABLE stats ADD COLUMN IF NOT EXISTS video_send_packet_loss INTEGER NOT NULL;
