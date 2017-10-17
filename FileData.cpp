#include "FileData.h"
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>

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

	HANDLE hFile = CreateFileA(filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if (hFile == INVALID_HANDLE_VALUE) return false;

	DWORD lo, hi;
	lo = hi = 0;
	lo = GetFileSize(hFile, &hi);
	size = lo | ((size_t)hi << 32);
	data = (char *)malloc(size);

	BOOL b = ReadFile(hFile, data, (DWORD)size, &lo, NULL);
	CloseHandle(hFile);
	if (!b) {
		close();
		return false;
	}
	strncpy_s(this->filename, filename, sizeof(this->filename));
	return true;
}

void FileData::close()
{
	if (data) {
		free(data);
		data = nullptr;
		index = 0;
		size = 0;
	}
}

bool FileData::getc(char & c)
{
	if (!data) return false;
	if (index >= size) return false;
	c = data[index];
	if (c == '\n') {
		nline++;
		ncol = 1;
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
	loc.line = nline;
	loc.col = ncol;
}
