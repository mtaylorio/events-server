module Events.Microseconds
  ( module Events.Microseconds
  ) where


microsecond :: Int
microsecond = 1


millisecond :: Int
millisecond = 1000 * microsecond


second :: Int
second = 1000 * millisecond


minute :: Int
minute = 60 * second
