ALTER TABLE rooms ADD COLUMN friendly_id TEXT NOT NULL;
ALTER TABLE rooms ADD CONSTRAINT rooms_friendly_id_key UNIQUE(friendly_id);
