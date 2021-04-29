
#if defined(WIN32)
 #include <windows.h>
const int path_separator = '\\';
#define strdup _strdup
#else
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
const int path_separator = '/';
#endif

#include <assert.h>

#include "FileObject.h"


FileObject::~FileObject()
{
}

FileObject::FileObject()
{
}

FileObject::FileObject(const char * file)
{
    setFile(file);
}

FileObject::FileObject(const FileObject & other)
{
    setFile(other.full_path);
}

void FileObject::setFile(const char * file)
{
#if defined(WIN32)
    auto res = GetFullPathNameA(file, sizeof(full_path), full_path, nullptr);
    if (res < 1) {
        strncpy(full_path, file, sizeof(full_path));
    }
#else 
    char *otherpath = realpath(file, full_path);
    if (otherpath != full_path) {
        strncpy(full_path, file, sizeof(full_path));
    }
#endif
}

char * FileObject::getFilename()
{
    return full_path;
}

void FileObject::copy(const FileObject & other)
{
    setFile(other.full_path);
}

void FileObject::setExtension(const char * extension)
{
    char *p = full_path + strlen(full_path);
    while ((*p != '.') && (p != full_path)) p--;
    if (extension && *extension != 0) p++;
    while (*extension) {
        *p = *extension;
        p++; extension++;
    }
    *p = 0;
}

char * FileObject::getRootPath()
{
    char *result = strdup(full_path);
    char *p = result + strlen(result);
    while ((*p != path_separator) && (p != result)) {
        *p = 0;
        p--;
    }
    *p = 0;
    return result;
}

char * FileObject::getName()
{
    char *p = full_path + strlen(full_path);
    while ((*p != path_separator) && (p != full_path)) p--;
    p++;
    return p;
}