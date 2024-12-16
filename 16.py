import heapq

INPUT = "16.input"
START = "S"
END = "E"
WALL = "#"
VOID = "."

EAST = (1, 0)
WEST = (-1, 0)
SOUTH = (0, 1)
NORTH = (0, -1)

FORWARD = 1
ROTATION = 1000

ROTATE = {
  NORTH: EAST,
  EAST: SOUTH,
  SOUTH: WEST,
  WEST: NORTH,
}

maze = []
start = None
end = None
with open(INPUT) as file:
  for (y, line) in enumerate(file):
    row = []
    for (x, char) in enumerate(line.strip()):
      row.append(char)
      if (char == START): start = (EAST, x, y)
      if (char == END): end = (x, y)
    maze.append(row)

costs = {(start[1], start[2]): 0}
pq = [(0, start)]

while pq:
  cost, vertice = heapq.heappop(pq)
  direction, x, y = vertice
  if cost <= costs[(x, y)]:
    cost_rotations = (
      (cost + FORWARD, direction),
      (cost + FORWARD + ROTATION, ROTATE[direction]),
      (cost + FORWARD + ROTATION + ROTATION, ROTATE[ROTATE[direction]]),
      (cost + FORWARD + ROTATION, ROTATE[ROTATE[ROTATE[direction]]]),
    )
    for (ncost, rotation) in cost_rotations:
      nx = x + rotation[0]
      ny = y + rotation[1]
      n = maze[ny][nx]
      ccost = costs.get((nx, ny))
      if n != WALL and (ccost == None or ccost > ncost):
        costs[(nx, ny)] = ncost
        heapq.heappush(pq, (ncost, (rotation, nx, ny)))

print(costs[end])