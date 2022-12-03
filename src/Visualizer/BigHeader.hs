module Visualizer.BigHeader
(fontChar)
where

fontChar :: Char -> String
fontChar 'a' = "\n\n  __ _  \n / _` | \n| (_| | \n \\__,_| \n\n\n"
fontChar 'b' = " _     \n| |    \n| |__  \n| '_ \\ \n| |_) |\n|_.__/  \n\n\n"
fontChar 'c' = "\n\n  ___ \n / __|\n| (__ \n \\___|\n\n\n"
fontChar 'd' = "     _ \n    | |\n  __| |\n / _` |\n| (_| |\n \\__,_|\n\n\n"
fontChar 'e' = "\n\n  ___ \n / _ \\\n|  __/\n \\___|\n\n\n"
fontChar 'f' = "  __ \n / _|\n| |_ \n|  _|\n| |  \n|_|  \n\n\n"
fontChar 'g' = "\n\n  __ _ \n / _` |\n| (_| |\n \\__, |\n  __/ |\n |___/ \n"
fontChar 'h' = " _     \n| |    \n| |__  \n| '_ \\ \n| | | |\n|_| |_|\n\n\n"
fontChar 'i' = " _ \n(_)\n _ \n| |\n| |\n|_|\n\n\n"
fontChar 'j' = "   _ \n  (_)\n   _ \n  | |\n  | |\n  | |\n _/ |\n|__/ \n"
fontChar 'k' = " _    \n| |   \n| | __\n| |/ /\n|   < \n|_|\\_\\\n\n\n"
fontChar 'l' = " _ \n| |\n| |\n| |\n| |\n|_|\n\n\n"
fontChar 'm' = "\n\n _ __ ___  \n| '_ ` _ \\ \n| | | | | |\n|_| |_| |_|\n\n\n"
fontChar 'n' = "\n\n _ __  \n| '_ \\ \n| | | |\n|_| |_|\n\n\n"
fontChar 'o' = "\n\n  ___  \n / _ \\ \n| (_) |\n \\___/ \n\n\n"
fontChar 'p' = "\n\n _ __  \n| '_ \\ \n| |_) |\n| .__/ \n| |    \n|_|    \n"
fontChar 'q' = "\n\n  __ _ \n / _` |\n| (_| |\n \\__, |\n    | |\n    |_|\n"
fontChar 'r' = "\n\n _ __ \n| '__|\n| |   \n|_|   \n\n\n"
fontChar 's' = "\n\n ___ \n/ __|\n\\__ \\\n|___/\n\n\n"
fontChar 't' = " _   \n| |  \n| |_ \n| __|\n| |_ \n \\__|\n\n\n"
fontChar 'u' = "\n\n _   _ \n| | | |\n| |_| |\n \\__,_|\n\n\n"
fontChar 'v' = "\n\n__   __\n\ \ / /\n \\ V / \n  \\_/  \n\n\n"
fontChar 'w' = "\n\n__      __\n\\ \\ /\\ / /\n \\ V  V / \n  \\_/\\_/  \n\n\n"
fontChar 'x' = "\n\n__  __\n\\ \\/ /\n >  < \n/_/\\_\\\n\n\n"
fontChar 'y' = "\n\n _   _ \n| | | |\n| |_| |\n \\__, |\n  __/ |\n |___/ \n"
fontChar 'z' = "\n\n ____\n|_  /\n / / \n/___|\n\n\n"
fontChar ' ' = " \n \n \n \n \n \n \n \n "
fontChar s = show s
