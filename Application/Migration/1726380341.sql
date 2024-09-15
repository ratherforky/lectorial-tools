ALTER TABLE rooms ADD COLUMN updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL;
CREATE FUNCTION set_updated_at_to_now() RETURNS TRIGGER AS $$BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;$$ language PLPGSQL;
CREATE TRIGGER update_rooms_updated_at BEFORE UPDATE ON rooms FOR EACH ROW EXECUTE FUNCTION set_updated_at_to_now();
