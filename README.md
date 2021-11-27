# AvailabilityPropertyChecker
Property-based test for critical scheduling logic.

* The structure is sound if and only if all hold true:
  1. A hash can be used to retrieve availability for the time slot represented by the hash.
  2. For all IDs representing those with availability within the given date range, the ID along with the hash representing a given time slot can be used to verify that the ID is listed in the given time slot.
* The algorithm used to generate hashes that represent time slots is correct if and only if all hold true:
  1. The algorithm is referentially transparent.
  2. No two different time slots will produce the same hash.
