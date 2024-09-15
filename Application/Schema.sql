CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language plpgsql;
-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE rooms (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    friendly_id TEXT NOT NULL UNIQUE,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX rooms_created_at_index ON rooms (created_at);
CREATE TABLE students (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL UNIQUE,
    username TEXT NOT NULL
);
CREATE TABLE rooms_students (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    room_id UUID NOT NULL,
    student_id UUID NOT NULL,
    in_answer_pool BOOLEAN NOT NULL
);
CREATE INDEX rooms_students_room_id_index ON rooms_students (room_id);
CREATE INDEX rooms_students_student_id_index ON rooms_students (student_id);
CREATE TABLE rooms_students_selected (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    room_id UUID NOT NULL,
    student_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX rooms_students_selected_room_id_index ON rooms_students_selected (room_id);
CREATE INDEX rooms_students_selected_student_id_index ON rooms_students_selected (student_id);
CREATE INDEX rooms_students_selected_created_at_index ON rooms_students_selected (created_at);
CREATE TRIGGER update_rooms_updated_at BEFORE UPDATE ON rooms FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
ALTER TABLE rooms_students ADD CONSTRAINT rooms_students_ref_room_id FOREIGN KEY (room_id) REFERENCES rooms (id) ON DELETE NO ACTION;
ALTER TABLE rooms_students ADD CONSTRAINT rooms_students_ref_student_id FOREIGN KEY (student_id) REFERENCES students (id) ON DELETE NO ACTION;
ALTER TABLE rooms_students_selected ADD CONSTRAINT rooms_students_selected_ref_room_id FOREIGN KEY (room_id) REFERENCES rooms (id) ON DELETE NO ACTION;
ALTER TABLE rooms_students_selected ADD CONSTRAINT rooms_students_selected_ref_student_id FOREIGN KEY (student_id) REFERENCES students (id) ON DELETE NO ACTION;
