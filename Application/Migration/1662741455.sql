CREATE TABLE rooms (
    id TEXT PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
ALTER TABLE rooms ADD CONSTRAINT rooms_id_key UNIQUE(id);
CREATE INDEX rooms_created_at_index ON rooms (created_at);
CREATE TABLE students (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    username TEXT NOT NULL
);
CREATE TABLE rooms_students (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    room_id UUID NOT NULL,
    student_id UUID NOT NULL,
    in_answer_pool BOOLEAN DEFAULT false NOT NULL
);
CREATE INDEX rooms_students_room_id_index ON rooms_students (room_id);
CREATE INDEX rooms_students_student_id_index ON rooms_students (student_id);
ALTER TABLE rooms_students ADD CONSTRAINT rooms_students_ref_room_id FOREIGN KEY (room_id) REFERENCES rooms (id) ON DELETE NO ACTION;
ALTER TABLE rooms_students ADD CONSTRAINT rooms_students_ref_student_id FOREIGN KEY (student_id) REFERENCES students (id) ON DELETE NO ACTION;
