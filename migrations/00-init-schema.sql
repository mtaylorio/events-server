

CREATE TABLE "topics" (
  "uuid" UUID NOT NULL,
  "broadcast" BOOLEAN NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  PRIMARY KEY ("uuid")
);


CREATE TABLE "events" (
  "uuid" UUID NOT NULL,
  "topic_uuid" UUID NOT NULL,
  "created_at" TIMESTAMP NOT NULL,
  "payload" JSONB NOT NULL,
  PRIMARY KEY ("uuid"),
  FOREIGN KEY ("topic_uuid") REFERENCES "topics" ("uuid")
);
