ALTER TABLE rooms DROP COLUMN id;
ALTER TABLE rooms ADD COLUMN id UUID DEFAULT uuid_generate_v4() NOT NULL;
ALTER TABLE rooms_students DROP COLUMN room_id;
ALTER TABLE rooms_students ADD COLUMN room_id UUID NOT NULL;
ALTER TABLE rooms_students_selected DROP COLUMN room_id;
ALTER TABLE rooms_students_selected ADD COLUMN room_id UUID NOT NULL;
ALTER TABLE students ADD CONSTRAINT students_id_key UNIQUE(id);
