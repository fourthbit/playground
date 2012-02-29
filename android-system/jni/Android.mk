
LOCAL_PATH := $(call my-dir)

# If compiling cairo & pixman:
#include jni/pixman.mk
#include jni/cairo.mk
include $(CLEAR_VARS)

LOCAL_MODULE := native-activity
LOCAL_SRC_FILES := constants-rt.c cairo-rt.c resources_1-rt.c audio_1-rt.c graphics_1-rt.c input_1-rt.c anygame_1-rt.c main_1-rt.c main.c
LOCAL_CFLAGS += -O2 -fno-short-enums -Wno-missing-field-initializers 
LOCAL_C_INCLUDES += /data/projects/android/android-ndk-r7b/platforms/android-14/arch-arm/usr/include /usr/include /usr/local/Gambit-C/include/ jni/cairo-extra /usr/include/cairo jni/pixman-extra /usr/include/pixman
LOCAL_CXXFLAGS := $(LOCAL_CFLAGS)
# If compiling cairo & pixman:
#LOCAL_LDLIBS := -lm -llog -landroid -lEGL -lGLESv1_CM -ldl -fno-short-enums -lc -L./jni/ext -lgambc
LOCAL_LDLIBS := -lm -llog -landroid -lEGL -lGLESv1_CM -ldl -fno-short-enums -lc -L./jni/ext -lgambc -lcairo -lpixman
LOCAL_STATIC_LIBRARIES := android_native_app_glue libcairo libpixman

include $(BUILD_SHARED_LIBRARY)

$(call import-module,android/native_app_glue)
