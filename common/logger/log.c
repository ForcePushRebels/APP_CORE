// SPDX-License-Identifier: LicenseRef-PATO-ESEO
// SPDX-Licence-CopyrightText: PATO ESEO License (see LICENSE.md)

#include "log.h"

#ifdef _WIN32
    #include <windows.h>
#else
    #include <unistd.h>
    #include <sys/ioctl.h>
    #include <sys/time.h>
#endif

static unsigned int log_count = 0;

// Platform-agnostic terminal width getter
static int get_terminal_width(void) {
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) {
        return csbi.srWindow.Right - csbi.srWindow.Left + 1;
    }
#else
    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != -1) {
        return w.ws_col;
    }
#endif
    return 80;
}

static void get_timestamp(char *buffer, size_t size) {
#ifdef _WIN32
    SYSTEMTIME st;
    GetLocalTime(&st);
    snprintf(buffer, size, "%02d:%02d:%02d.%03d",
             st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    struct tm *tm_info = localtime(&tv.tv_sec);
    snprintf(buffer, size, "%02d:%02d:%02d.%03ld",
             tm_info->tm_hour, tm_info->tm_min, tm_info->tm_sec, tv.tv_usec / 1000);
#endif
}

void log_print(const char *level, const char *tag, const char *fmt, ...) {
    int term_width = get_terminal_width();
    int indent_width = term_width / LOG_INDENT_STEPS;
    int indent = (log_count++ % LOG_INDENT_STEPS) * indent_width;

    for (int i = 0; i < indent; ++i) fputs(ASCII_SPACE, stdout);

	char time_buf[13]; // HH:MM:SS.mmm
	get_timestamp(time_buf, sizeof(time_buf));

	printf("%s %s (%s) ", level, time_buf, tag);

    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    va_end(args);

    fputs(ASCII_NL ASCII_NL, stdout);  // Double line break for readability
}