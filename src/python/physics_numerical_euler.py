# File: src/python/physics_numerical_euler.py

# Simulates an object being launched at a particular angle given an initial velocity using the forward Euler method.

import math

ANGLE = 30.0 #degrees
V0 = 112.0 #initial velocity, m/s
DT = 0.01
G = -9.8 #gravity, m/s
H_DAMP = 2.0 #wind, m/s

t = 0.0 #time (seconds)
x = 0.0
y = 0.0

vx = V0 * math.sin(math.radians(ANGLE))
vy = V0 * math.sin(math.radians(ANGLE))

def vel(v_x, v_y, a_x, a_y, dt):
	v_x_new = v_x + (a_x * dt)
	v_y_new = v_y + (a_y * dt)

	return (v_x_new, v_y_new)

def pos(pos_x, pos_y, v_x, v_y, dt):
	x_new = pos_x + (v_x * dt)
	y_new = pos_y + (v_y * dt)

	return (x_new, y_new)

outfile = open("euler.txt", "w")
while True:
	vx, vy = vel(vx, vy, H_DAMP, G, DT)
	x, y = pos(x, y, vx, vy, DT)

	t += DT

	if y <= 0:
		break

	outfile.write("%s %s %s %s %s\n" % (t, x, y, vx, vy))
outfile.close()
