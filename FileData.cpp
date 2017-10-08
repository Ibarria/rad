#include "FileData.h"
#include <windows.h>
#include <stdlib.h>

FileData::FileData()
{
	data = nullptr;
	index = 0;
	size = 0;
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

void FileData::rewind_one()
{
	if (index > 0) index--;
}
