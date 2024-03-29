# -*- coding: utf-8 -*-
"""fine-tune-2-class.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1vR4EEHpMRSIy1-DWlFsaUQ5NKZEPrZCA
"""

import numpy as np
import h5py as h5py
import matplotlib.pyplot as plt
import tensorflow as tf
 
from keras.preprocessing.image import ImageDataGenerator
from keras.models import Sequential, Model
from keras.layers import Dropout, Flatten, Dense
from keras import applications, optimizers
from PIL import Image
from sklearn.metrics import classification_report, confusion_matrix
 
 
# dimensions of our images.
img_width, img_height = 160, 224
 
#top_model_weights_path = 'bottleneck_VGG16_00.h5'
train_data_dir = '/content/drive/MyDrive/CEPE/dementiaproject/2-class-bal-more-cut/data_augment_train'
validation_data_dir = '/content/drive/MyDrive/CEPE/dementiaproject/2-class-bal-more-cut/data_augment_valid'
test_data_dir = '/content/drive/MyDrive/CEPE/dementiaproject/2-class-bal-more-cut/data_augment_test'
nb_train_samples = 6997 #3584
nb_validation_samples = 2254 #1224 #2795
nb_test_samples = 5225 #2684
epochs = 100
batch_size = 40

train_datagen = ImageDataGenerator(
    rescale=1. / 255, 
    rotation_range=10,
    width_shift_range=0.1,
    height_shift_range=0.1,
    shear_range=0.15,
    zoom_range=0.2,
    channel_shift_range = 150,
    fill_mode='nearest')

test_datagen = ImageDataGenerator(rescale=1. / 255)

train_generator = train_datagen.flow_from_directory(
    train_data_dir,
    target_size=(img_height, img_width),
    batch_size=batch_size,
    class_mode='binary')

validation_generator = test_datagen.flow_from_directory(
    validation_data_dir,
    target_size=(img_height, img_width),
    batch_size=batch_size,
    class_mode='binary')

test_generator = test_datagen.flow_from_directory(
    test_data_dir,
    target_size=(img_height, img_width),
    batch_size=4,
    shuffle = False,
    class_mode='binary')

"""Architecture modèle"""

np.random.seed(2929)

vgg_model = applications.VGG16(weights='imagenet', include_top=False, input_shape=(img_width, img_height, 3))
print('Model loaded.')

# initialise top model
top_model = Sequential()
top_model.add(Flatten(input_shape=vgg_model.output_shape[1:]))
top_model.add(Dense(256, activation='relu'))
top_model.add(Dropout(0.5))
top_model.add(Dense(1, activation='sigmoid'))

model = Model(inputs=vgg_model.input, outputs=top_model(vgg_model.output))

model.trainable = True
# first train only classif part
for layer in model.layers[:19]:
    layer.trainable = False
    
model.summary()

sgd = optimizers.Adam(lr=2e-5)  # optimizers.SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)

model.compile(loss="binary_crossentropy",
              optimizer=sgd,
              metrics=['accuracy']
              )

"""Training extract feature :"""

early_stopping_cb = tf.keras.callbacks.EarlyStopping(patience=20,
                                                     restore_best_weights=True)
history = model.fit_generator(
    train_generator,
    steps_per_epoch=nb_train_samples // batch_size,
    epochs=epochs,
    validation_data=validation_generator,
    validation_steps=nb_validation_samples // batch_size,
    callbacks = early_stopping_cb,
    verbose=1)

history_dict = history.history

model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/')
model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/vgg_2e5_more_cut.h5')

import pandas as pd

# convert the history.history dict to a pandas DataFrame:     
hist_df = pd.DataFrame(history.history) 

# save to json:  
hist_json_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/history.json' 
with open(hist_json_file, mode='w') as f:
    hist_df.to_json(f)

# or save to csv: 
hist_csv_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/history.csv'
with open(hist_csv_file, mode='w') as f:
    hist_df.to_csv(f)


# Plotting the training and validation loss
history_dict = history.history
loss_values = history_dict['loss']
val_loss_values = history_dict['val_loss']
epochs_0 = range(1, len(history_dict['accuracy']) + 1)
plt.plot(epochs_0, loss_values, 'bo', label='Training loss')
plt.plot(epochs_0, val_loss_values, 'b', label='Validation loss')
plt.title('Training and validation loss (feature-extract)')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.show()

# Plotting the training and validation accuracy
acc_values = history_dict['accuracy']
val_acc_values = history_dict['val_accuracy']
plt.plot(epochs_0, acc_values, 'bo', label='Training acc')
plt.plot(epochs_0, val_acc_values, 'b', label='Validation acc')
plt.title('Training and validation accuracy (feature-extract)')
plt.xlabel('Epochs')
plt.ylabel('Accuracy')
plt.legend()
plt.show()

# save / load model feature extraction
model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/')
model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/vgg_2e5_more_cut.h5')

import pandas as pd

# convert the history.history dict to a pandas DataFrame:     
hist_df = pd.DataFrame(history.history) 

# save to json:  
hist_json_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/history.json' 
with open(hist_json_file, mode='w') as f:
    hist_df.to_json(f)

# or save to csv: 
hist_csv_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/history.csv'
with open(hist_csv_file, mode='w') as f:
    hist_df.to_csv(f)


#from keras.models import load_model
#model = load_model('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/model_feature_extract_2_class_bal/model_feature_extract_2_class_bal.h5')

from keras.models import load_model
model = load_model('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/feature_extract_2_class_bal/vgg_2e5_more_cut/vgg_2e5_more_cut.h5')
model.summary()

"""Training Fine-tuning"""

#Fine tunning


# Total of 20 layers. The classification is considered as one layer
# Therefore, intermediate is 19 layers
# 0, 1[:4], 2[:7], 3[:11], 4[:15], 5[:19] (Group 0, 1, 2, 3, 4, 5)
# 0 -> All trainable
# 5 -> All non-trainable except classification layer
# Always keep layer 20 trainable because it is classification layer
model.trainable = True

for layer in model.layers[:7]:
       layer.trainable = False

model.summary()

sgd = optimizers.Adam(lr=1e-6)  # optimizers.SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)

model.compile(loss="binary_crossentropy",
              optimizer=sgd,
              metrics=['accuracy']
              )

early_stopping_cb = tf.keras.callbacks.EarlyStopping(patience=20,
                                                     restore_best_weights=True)
history = model.fit_generator(
    train_generator,
    steps_per_epoch=nb_train_samples // batch_size,
    epochs=epochs,
    validation_data=validation_generator,
    validation_steps=nb_validation_samples // batch_size,
    callbacks = early_stopping_cb,
    verbose=1)


history_dict = history.history

# Plotting the training and validation loss
history_dict = history.history
loss_values = history_dict['loss']
val_loss_values = history_dict['val_loss']
epochs_0 = range(1, len(history_dict['accuracy']) + 1)
plt.plot(epochs_0, loss_values, 'bo', label='Training loss')
plt.plot(epochs_0, val_loss_values, 'b', label='Validation loss')
plt.title('Training and validation loss (fine-tuning)')
plt.xlabel('Epochs')
plt.ylabel('Loss')
plt.legend()
plt.show()
#plt.savefig('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/model3loss.png')
#plt.close()

# Plotting the training and validation accuracy
acc_values = history_dict['accuracy']
val_acc_values = history_dict['val_accuracy']
plt.plot(epochs_0, acc_values, 'bo', label='Training acc')
plt.plot(epochs_0, val_acc_values, 'b', label='Validation acc')
plt.title('Training and validation accuracy (fine-tuning)')
plt.xlabel('Epochs')
plt.ylabel('Accuracy')
plt.legend()
plt.show()
#plt.savefig('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/model3acc.png')
#plt.close()

# save / load model feature extraction
model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/fine_tuning_2_class_bal/vgg_2e5_more_cut_3block/')
model.save('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/fine_tuning_2_class_bal/vgg_2e5_more_cut_3block/vgg_2e5_more_cut_3block.h5')

import pandas as pd

# convert the history.history dict to a pandas DataFrame:     
hist_df = pd.DataFrame(history.history) 

# save to json:  
hist_json_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/fine_tuning_2_class_bal/vgg_2e5_more_cut_3block/history.json' 
with open(hist_json_file, mode='w') as f:
    hist_df.to_json(f)

# or save to csv: 
hist_csv_file = '/content/drive/MyDrive/CEPE/dementiaproject/models_colab/fine_tuning_2_class_bal/vgg_2e5_more_cut_3block/history.csv'
with open(hist_csv_file, mode='w') as f:
    hist_df.to_csv(f)

print("prev")
import numpy

#model = load_model('/content/drive/MyDrive/CEPE/dementiaproject/models_colab/model_7.h5')

test_steps_per_epoch = numpy.math.ceil(test_generator.samples / test_generator.batch_size)

#predictions = model.predict_generator(
#    generator=test_generator,
#    steps = test_steps_per_epoch,
#    verbose = 1)

predictions = model.predict(
    test_generator,
    steps = test_steps_per_epoch,
    verbose =1)

predicted_classes = (predictions > 0.5).astype("int32")

#print(predictions)
#predicted_classes = numpy.argmax(predictions, axis=1)
print(predicted_classes)
true_classes = test_generator.classes
print(true_classes)
class_labels = list(test_generator.class_indices.keys())   
print(class_labels)

import sklearn
report = sklearn.metrics.classification_report(true_classes, predicted_classes, target_names=class_labels)
conf = sklearn.metrics.confusion_matrix(true_classes, predicted_classes)
print(report)   
print(conf)

test_generator.filenames[-2]

eval = model.evaluate_generator(
    test_generator,
    steps = nb_test_samples // batch_size,
    verbose = 1
)

print(eval)

eval_val = model.evaluate_generator(
    validation_generator,
    steps = nb_validation_samples // batch_size,
        verbose = 1
)

print(eval_val)

# Commented out IPython magic to ensure Python compatibility.
#visualisation 

from vis.visualization import visualize_activation
from vis.utils import utils
from keras import activations

from matplotlib import pyplot as plt
# %matplotlib inline