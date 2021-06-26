import numpy as np
import math

def cluster(points, metric, r):
    clusters = [[p] for p in points]
    prev_clusters = []
    while (clusters != prev_clusters):
        prev_clusters = clusters
        i = 0
        while (i < len(clusters)):
            j = i + 1
            while (j < len(clusters)):
                if (has_near(clusters[i], clusters[j], metric, r)):
                    clusters[j] = union(clusters[i], clusters[j])
                    clusters[i] = []
                j += 1
            i += 1

    remove_empty(clusters)
    return clusters

def has_near(A, B, metric, r):
    for x in A:
        for y in B:
            if metric(x,y) < r:
                return True
    return False

def remove_empty(l):
    i = 0
    while i < len(l):
        if l[i] == []:
            l.pop(i)
            i -= 1
        i += 1

def unique(l):
    return list(set(l))

def union(A, B):
    return unique(A+B)

def m(p, q):
    return math.sqrt((p[0] - q[0])**2 + (p[1] - q[1])**2)

print(cluster([(2,2), (2,3), (3,4), (0,0), (1,0), (0, 1)], m, 2))
