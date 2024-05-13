#pragma once

void trace(const char *fmt, ...)
    __attribute__((format (printf, 1, 2)));
