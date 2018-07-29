#include "FileData.h"
#ifdef WIN32
# include <windows.h>
#else
# define strncpy_s strncpy
# include <string.h>
#endif
#include <stdlib.h>
#include <stdio.h>

#if defined(getc)
#undef getc
#endif

FileData::FileData()
{
	data = nullptr;
	index = 0;
	size = 0;
	nline = 1;
	ncol = 1;
}

FileData::~FileData()
{
	close();
}

bool FileData::open(const char * filename)
{
	close();
#ifdef WIN32
	HANDLE hFile = CreateFileA(filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (hFile == INVALID_HANDLE_VALUE) return false;

	DWORD lo, hi;
	lo = hi = 0;
	lo = GetFileSize(hFile, &hi);
	size = lo | ((size_t)hi << 32);
	data = (char *)malloc(size);

	BOOL b = ReadFile(hFile, data, (DWORD)size, &lo, NULL);
	CloseHandle(hFile);
#else
	FILE *hFile = fopen(filename, "r");
	if (!hFile) return false;
	
	fseek(hFile, 0, SEEK_END);
	size = ftell(hFile);
	data = (char *)malloc(size);
	fseek(hFile, 0, SEEK_SET);
	
	bool b = (size == fread(data, 1, size, hFile));
#endif	
	if (!b) {
		close();
		return false;
	}
    lines.push_back(data);
	strncpy_s(this->filename, filename, sizeof(this->filename));
	return true;
}

bool FileData::loadString(const char *str, u64 num_chars)
{
	close();
	const char *fakename = "In Place String";
	data = (char *) malloc(num_chars);
	memcpy(data, str, num_chars);
	size = num_chars;
	strncpy_s(this->filename, fakename, strlen(fakename) );	

	return true;
}

void FileData::close()
{
	if (data) {
		free(data);
		data = nullptr;
        lines.reset();
	}
	index = 0;
	size = 0;	
    nline = 1;
    ncol = 1;
}

bool FileData::getc(char & c)
{
	if (!data) return false;
	if (index >= size) return false;
	c = data[index];
	if (c == '\n') {
		nline++;
		ncol = 1;
        lines.push_back(&data[index + 1]);
	} else {
		ncol++;
	}
	index++;
	return true;
}

bool FileData::peek(char & c)
{
	if (!data) return false;
	if (index >= size) return false;
	c = data[index];
	return true;
}

void FileData::getLocation(SrcLocation & loc) const
{
	loc.line = (u32)nline;
	loc.col = (u32)ncol;
}

void FileData::lookAheadTwo(char * in)
{
    if (!data) return;
    if (index < size) in[0] = data[index];
    if (index +1 < size) in[1] = data[index+1];
}

char * FileData::printLocation(const SrcLocation & loc, char *str) const
{
    if (loc.line > lines.size()) {
        s32 off = sprintf(str, "Wrong location: %s : %d,%d\n", filename, loc.line, loc.col);
        assert(false);
        return str + off;
    }

    // How to print: print one line above, the current line, the marker
    if (loc.line > 1) {
        // -1 for previous, -1 because lines is 0 indexed
        char *prev_line = lines[loc.line - 2];
        char *end = strchr(prev_line, '\n');
        s32 off = sprintf(str, "%.*s", (u32)(end - prev_line + 1), prev_line);
        str += off;
    }

    {
        char *cur_line = lines[loc.line - 1];
        char *end = strchr(cur_line, '\n');
        s32 off = sprintf(str, "%.*s", (u32)(end - cur_line +1), cur_line);
        str += off;
    }

    {
        // draw the marker
        if (loc.col <= 16) {
            // small column, marker looks like:
            //   ^-----------
            s32 off = sprintf(str, "%*s^%s\n", loc.col - 1, "", "----------------");
            str += off;
        } else {
            // small column, marker looks like:
            //   -----------^
            s32 off = sprintf(str, "%*s%s^\n", (loc.col - 17), "", "----------------");
            str += off;
        }
    }
    return str;
}
