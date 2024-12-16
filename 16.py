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

costs = {start: 0}
pq = [(0, start)]
path = {}

while pq:
  cost, vertice = heapq.heappop(pq)
  direction, x, y = vertice
  if cost <= costs[vertice]:
    cost_rotations = (
      (cost + FORWARD, direction),
      (cost + FORWARD + ROTATION, ROTATE[direction]),
      (cost + FORWARD + ROTATION, ROTATE[ROTATE[ROTATE[direction]]]),
    )
    for (ncost, rotation) in cost_rotations:
      nx = x + rotation[0]
      ny = y + rotation[1]
      n = maze[ny][nx]
      nvertice = (rotation, nx, ny)
      ccost = costs.get(nvertice)
      if n != WALL and (ccost == None or ccost > ncost):
        path[nvertice] = set([vertice])
        costs[nvertice] = ncost
        heapq.heappush(pq, (ncost, nvertice))
      elif n != WALL and ccost == ncost:
        path[nvertice].add(vertice)
      

min_end = min([(EAST, end[0], end[1]),
               (WEST, end[0], end[1]),
               (NORTH, end[0], end[1]),
               (SOUTH, end[0], end[1])],
              key=lambda x: costs.get(x, float("inf")))

print(costs[min_end])

hops = {min_end, start}
points = {min_end}
while points:
  point = points.pop()
  for p in path[point]:
    if p not in hops:
      points.add(p)
      hops.add(p)

hh = set()
for (d, x, y) in hops:
  hh.add((x,y))

for (y, row) in enumerate(maze):
  for (x, ch) in enumerate(row):
    if ((x, y) in hh):
      print("0", end="")
    else:
      print(ch, end="")
  print("")

print(len(hh))