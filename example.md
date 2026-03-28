    type number = int | float
    type vector =
        number x, y, z = 0

    on (vector v) = (vector a) + (vector b)
        v = vector(a.x + b.x, a.y + b.y, a.z + b.z)

    on (number n) = smaller of (number a) and (number b)
        n = (a) if (a < b) else (b)
