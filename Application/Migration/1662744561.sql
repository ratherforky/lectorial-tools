CREATE TABLE rooms_students_selected (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    room_id TEXT NOT NULL,
    student_id UUID NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX rooms_students_selected_room_id_index ON rooms_students_selected (room_id);
CREATE INDEX rooms_students_selected_student_id_index ON rooms_students_selected (student_id);
CREATE INDEX rooms_students_selected_created_at_index ON rooms_students_selected (created_at);
ALTER TABLE rooms_students_selected ADD CONSTRAINT rooms_students_selected_ref_room_id FOREIGN KEY (room_id) REFERENCES rooms (id) ON DELETE NO ACTION;
ALTER TABLE rooms_students_selected ADD CONSTRAINT rooms_students_selected_ref_student_id FOREIGN KEY (student_id) REFERENCES students (id) ON DELETE NO ACTION;
