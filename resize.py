import cv2

cur = cv2.imread("image.png")
resized = cv2.resize(cur, (30, 30))
cv2.imwrite("issei.png", resized)
