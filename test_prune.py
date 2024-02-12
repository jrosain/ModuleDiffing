import sys

matching = [('1', '51'), ('2', '53'), ('2', '58'), ('3', '54'), ('3', '59'), ('4', '55'), ('5', '56'), \
            ('6', '52'), ('6', '57'), ('7', '61'), ('7', '63'), ('8', '62'), ('8', '64'), ('9', '61'), \
            ('9', '63'), ('10', '62'), ('10', '64'), ('+', '60'), ('+', '-')]
edges = []
for edge in sys.stdin.readline().split(' ') :
    if len(edge) > 0:
        a, b = edge.split(',')
        edges.append((a, b))
for a, b in matching :
    if (a, b) not in edges and (b, a) not in edges :
        print(f"ERROR: {a},{b} PRUNED WHILE IT SHOULD NOT BE")
    if (a, b) in edges :
        edges.remove((a, b))
    if (b, a) in edges:
        edges.remove((b, a))
print(*edges)
