import numpy as np
import h5py as h5py
import matplotlib.pyplot as plt
import tensorflow as tf
import sklearn

from keras.preprocessing.image import ImageDataGenerator
from keras.models import Sequential, Model
from keras.layers import Dropout, Flatten, Dense
from keras import applications, optimizers
from keras.models import load_model
from PIL import Image
from sklearn.metrics import classification_report, confusion_matrix
from numpy import asarray
from numpy import savetxt
from numpy import math

img_width, img_height = 160, 224
nb_test_samples = 4  
test_data_dir = '/srv/OASIS_DATA/data_base_shiny/png_tmp/'
batch_size = 1

test_datagen = ImageDataGenerator(rescale=1. / 255)
test_generator = test_datagen.flow_from_directory(
  test_data_dir,
  target_size=(img_height, img_width),
  batch_size=batch_size,
  shuffle = False,
  class_mode='binary')

model = load_model("/srv/OASIS_DATA/data_base_shiny//models/2_class_irm/vgg_2e5_more_cut_3block_run2.h5")

#test_steps_per_epoch = numpy.math.ceil(test_generator.samples / test_generator.batch_size)

predictions = model.predict(
    test_generator,
    steps = 4,
    verbose =1)

predicted_classes = (predictions > 0.5).astype("int32")
savetxt('/srv/OASIS_DATA/data_base_shiny//models/2_class_irm/prev.csv', predictions, delimiter=',')
