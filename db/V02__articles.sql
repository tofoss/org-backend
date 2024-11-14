CREATE TABLE articles (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id UUID NOT NULL REFERENCES users(id),
    title VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    published_at TIMESTAMPTZ,
    published BOOLEAN DEFAULT FALSE,
    tsv tsvector  -- Full-text search vector
);

CREATE INDEX idx_articles_user_id ON articles(user_id);
CREATE INDEX idx_articles_tsv ON articles USING gin(tsv);

