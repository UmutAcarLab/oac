#include <jni.h>
#include <string>
#include <iostream>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include "queso_api.h"

JavaVM* quesoVM;


struct ThreadData {
  JNIEnv* env;
  jobject wrapObj;
};

void startVM_ () {
  JNIEnv* env;
  JavaVMInitArgs vmArgs;
  JavaVMOption options[2];
  const char* jarFilePath = "../../queso/SymbolicOptimizer-1.0-SNAPSHOT-jar-with-dependencies.jar";
  options[0].optionString = "-Djava.class.path=SymbolicOptimizer-1.0-SNAPSHOT-jar-with-dependencies.jar"; // Specify the path to your JAR file
  options[1].optionString = "--enable-preview"; // Specify the path to your JAR file
  vmArgs.version = JNI_VERSION_1_8;
  vmArgs.nOptions = 2;
  vmArgs.options = options;
  vmArgs.ignoreUnrecognized = JNI_FALSE;
  std::cout << "creating\n"<<std::endl;
  jint result = JNI_CreateJavaVM(&quesoVM, (void**)&env, &vmArgs);
  if (result != JNI_OK) {
    std::cerr << "Failed to create JVM" << std::endl;
    return NULL;
  }
  quesoVM->DetachCurrentThread();
}

void initialize_ (const char* eqset_fn_, const char* symb_eqset_fn_, unsigned char** td_store){
  JNIEnv* env;
  std::cout << "attaching\n"<<std::endl;
  jint result = quesoVM->AttachCurrentThread((void**)&env, NULL)
  if (result != JNI_OK) {
    std::cerr << "Failed to attach to quesoVM" << std::endl;
    return NULL;
  }

  jclass wrapClass = env->FindClass("Wrapper");
  jmethodID wrapConst = (env)->GetMethodID(wrapClass, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");
  jstring rulefile = (env)->NewStringUTF(eqset_fn_);
  jstring symbRulefile = (env)->NewStringUTF(symb_eqset_fn_);
  jobject wrapObj_ = (env)->NewObject(wrapClass, wrapConst, rulefile, symbRulefile);
  jobject wrapObj = env->NewGlobalRef (wrapObj_);
  std::cout << "wrapper class init (global ref) = " << wrapObj << std::endl;

  ThreadData* td = new ThreadData();
  td->env = env;
  td->wrapObj = wrapObj;
  *td_store = reinterpret_cast<unsigned char*> (td);
  return;
}

int write_qasm_to_buffer (const char* cqasm, char* buffer, int buff_size) {
  int blen = static_cast<int>(strlen(cqasm));
  if (blen > buff_size) {
    return -1 * blen;
  } else {
    strcpy(buffer, cqasm);
    return blen;
  }
}

int opt_circuit_ (const char* cqasm_, int timeout, char* buffer, int buff_size, unsigned char* td_) {
  ThreadData* td = (ThreadData*)td_;
  JNIEnv* env = td->env;
  jobject wrapObj = td->wrapObj;

  jclass wrapClass = env->FindClass("Wrapper");
  jmethodID optFunc = (env)->GetMethodID(wrapClass, "optimize", "(Ljava/lang/String;ZLjava/lang/Integer;)Ljava/lang/String;");

  jstring cqasm = (env)->NewStringUTF(cqasm_);
  jstring result = (jstring)(env)->CallObjectMethod(wrapObj, optFunc, cqasm, timeout < 0, abs(timeout));
  const char *resultStr = (env)->GetStringUTFChars(result, NULL);
  return write_qasm_to_buffer(resultStr, buffer, buff_size);
}

// void opt_circuit_ (JNIEnv* env, jobject wrapObject, jclass wrapClass, std::string filename) {
//   // Create an input file stream
//   std::ifstream inputFile(filename);

//   // Check if the file was successfully opened
//   if (!inputFile.is_open()) {
//       std::cerr << "Failed to open file: " << filename << std::endl;
//       return; // Return an error code
//   }

//   // Read the entire file into a string
//   std::stringstream buffer;
//   buffer << inputFile.rdbuf();
//   std::string fileContents = buffer.str();
//   std::cout << fileContents;
//   // Close the file
//   inputFile.close();

//   jmethodID optFunc = (env)->GetMethodID(wrapClass, "optimize", "(Ljava/lang/String;ZI)Ljava/lang/String;");
//   std::cout << "found ";
//   std::cout<<optFunc<<std::endl;

//   jstring cqasm = (env)->NewStringUTF(fileContents.c_str());

//   jclass integerClass = env->FindClass("java/lang/Integer");
//   jmethodID integerConstructor = env->GetMethodID(integerClass, "<init>", "(I)V");
//   jobject timeout = env->NewObject(integerClass, integerConstructor, timeout); // Replace 42 with your Integer value



//   // Call the processString function
//   jstring result = (jstring)(env)->CallObjectMethod(wrapObject, optFunc, cqasm, JNI_TRUE, timeout);

//   // Convert the result to a C string
//   const char *resultStr = (env)->GetStringUTFChars(result, NULL);
//   std::cout << resultStr << std::endl;

// }

// int main() {
//   JNIEnv* env = init();
//   optimize(env, wrapObj, wrapClass, "test-small.qasm");



//   jthrowable exc = env->ExceptionOccurred();
//   if (exc) {
//     env->ExceptionDescribe();
//     env->ExceptionClear();
//     std::cerr << "Exception occurred while loading class " << "Applier" << std::endl;
//   }
// }
