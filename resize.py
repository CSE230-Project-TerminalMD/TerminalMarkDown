import cv2

cur = cv2.imread("issei.png")
resized = cv2.resize(cur, (20, 20))
cv2.imwrite("issei.png", resized)
