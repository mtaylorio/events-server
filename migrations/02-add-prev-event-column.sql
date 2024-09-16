-- Add the new column to the topics table
ALTER TABLE "topics" ADD COLUMN "last_event_uuid" UUID;

-- Update the last_event_uuid for each topic
WITH latest_events AS (
  SELECT DISTINCT ON (topic_uuid)
    topic_uuid,
    uuid AS last_event_uuid
  FROM events
  ORDER BY topic_uuid, created_at DESC
)
UPDATE topics
SET last_event_uuid = latest_events.last_event_uuid
FROM latest_events
WHERE topics.uuid = latest_events.topic_uuid;

-- Add the prev column to the events table
ALTER TABLE "events" ADD COLUMN "prev" UUID;
