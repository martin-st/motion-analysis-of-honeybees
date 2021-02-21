import numpy as np
import os
import glob
import csv
import cv2 
import re
from statistics import mode 
import time as time_measure
from multiprocessing import Process
# Our Libs

processes = []

# Definitions
def init():
    global JUMP_FRAMES, ADJUST_GAMMA, TRHESHOLD 
    JUMP_FRAMES = 0 # If we want to jump the beginning
    ADJUST_GAMMA = True # adjust Gamma
    TRHESHOLD = 7
    return True

def analyse_video(VIDEONUMBER, NAME, file, x_actuator, y_actuator, JUMP_FRAMES, ADJUST_GAMMA, TRHESHOLD, LTABLE):
    start_time = time_measure.time()
    VIDEO_FILE = file
    capture = cv2.VideoCapture(VIDEO_FILE)
    # wait till video is loaded
    while not capture.isOpened():
        capture = cv2.VideoCapture(VIDEO_FILE)
        cv2.waitKey(1000)
        print("Wait for the header")
    # jump forward in frames
    capture.set(cv2.CAP_PROP_POS_FRAMES, JUMP_FRAMES)
    length = int(capture.get(cv2.CAP_PROP_FRAME_COUNT))
    print( "Video total frames: {}".format(length) )
    roi_length = 200
    x1 = x_actuator - roi_length
    x2 = x_actuator + roi_length
    y1 = y_actuator  - roi_length
    y2 = y_actuator  + roi_length
    print(x1, y1, x2, y2)

    # Check if video file exists
    if(os.path.isfile(VIDEO_FILE) == False):
        print("Error: No video file found")
        exit()
    
    w_pixel_array = []
    x_vec = []
    y_vec = []
    counter = 0
    time, values,  = [], []

    second_count = 0        # used to count frames
    n_white_pix_sum = 0     # helper variable to sum white pixel in n amount of frames
    try:
        while True:
            # read video
            flag, frame = capture.read()
            # Break if Video is not ready or Key Click
            if flag != True:
                #print "Frame is not ready"
                cv2.waitKey(1000)
                exportData(1, time, values, NAME, VIDEONUMBER)
                break
            if 0xFF & cv2.waitKey(10) == 27:
                exportData(2, time, values, NAME, VIDEONUMBER)
                break
            # Grab Region of Interest (+1 for failsafe reasons)
            
            roi = frame[y1:y2, x1:x2]
        

            pos_frame = capture.get(1)
            roi = cv2.cvtColor(roi, cv2.COLOR_BGR2GRAY) # change to grayscale
            #cv2.resizeWindow('image', 500, 500)
            #cv2.imshow('image', roi)
            #cv2.waitKey(1)            
            if (ADJUST_GAMMA == True):
                roi = adjustGamma(roi, LTABLE)

            # check if it was the first run otherwise img_history is same as input for first round
            try:
                img_history
            except:
                img_history = roi
            
            # calculate absdiff
            img_output = cv2.absdiff(roi, img_history)
            img_history = roi # set new history

            # Converting
            img_output = cv2.medianBlur(img_output, 1)
            _, img_output = cv2.threshold(img_output, TRHESHOLD, 255, cv2.THRESH_BINARY)

            # all pixels of image
            n_all_px = img_output.size
            # get all white pixels == changed pixels
            n_white_pix = np.sum(img_output == 255)
            # save into our helper variable
            n_white_pix_sum = n_white_pix_sum + n_white_pix
            # set our frame counter forward
            second_count += 1
            counter += 1
            
            if counter % 500 == 0:
                duration = time_measure.time() - start_time                
                print(f"I'm at approximately frame {counter}, analysing the last 500 frames took me {duration} seconds")
                start_time = time_measure.time()

            # if 10 Frames we calculate the difference
            if (second_count == 10):
                # mean and relative value to all pixels of the cropped frame
                relative_white = (n_white_pix_sum / second_count) /  n_all_px * 100

                # add value our vector
                y_vec.extend([relative_white])
                x_vec.extend([pos_frame])

                # move our vector forward
                if (len(x_vec) > 250):
                    y_vec.pop(0)
                    x_vec.pop(0)

                # reset helper
                n_white_pix_sum = 0
                second_count = 0
                
                time.append(pos_frame)
                values.append(relative_white)



    except KeyboardInterrupt:
        exportData(3, time, values, NAME, VIDEONUMBER)
        pass
  


def gammaTable():
    global LTABLE
    # build a lookup table mapping the pixel values [0, 255] to
    # their adjusted gamma values
    global LTABLE
    # Somehow I found the value of `gamma=1.2` to be the best in my case
    invGamma = 1.0 / 1.2
    LTABLE = np.array([((i / 255.0) ** invGamma) * 255
        for i in np.arange(0, 256)]).astype("uint8")
    return True


def adjustGamma(image, LTABLE):
    # apply gamma correction using the lookup table
    return cv2.LUT(image, LTABLE)

def exportData(position, time, values, NAME, VIDEONUMBER):
    data = zip(time, values)
    print(data)
    with open(f'{NAME}/result{position}_{VIDEONUMBER}_new.csv', mode='w', newline="") as result:
        result = csv.writer(result, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        for i in data: result.writerow(i)

def main():
    global ADJUST_GAMMA
    files = ['filepath1, filepath2, ... filepath_n'] 
    x_actuator = ['list of x-coordinates of emitter position']
    y_actuator = ['list of y-coordinates of emitter position']
    init()
    if(ADJUST_GAMMA == True):
        gammaTable()
    for i in range(len(files)):
        name, number = files[i].split(r'MVI')[-2], files[i].split(r'MVI_')[-1].split('.')[0]
        print(name)
        p = Process(target=analyse_video, args =(number, name, files[i], x_actuator[i], y_actuator[i], JUMP_FRAMES, ADJUST_GAMMA, TRHESHOLD, LTABLE))
        p.start()
        processes.append(p)
    for p in processes:
        p.join()

if __name__ == "__main__":
   main()
   
