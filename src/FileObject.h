#pragma once

class FileObject
{
    char full_path[512];
public:
    ~FileObject();
    FileObject();
    FileObject(const char *file);
    FileObject(const FileObject &other);
    void setFile(const char *file);
    char *getFilename();
    void copy(const FileObject &other);
    void setExtension(const char *extension);
    char *getRootPath();
    char *getName();
private:

};

