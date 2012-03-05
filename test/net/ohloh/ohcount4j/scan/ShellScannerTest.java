package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.LANG_SHELL;

import org.testng.annotations.Test;

public class ShellScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new ShellScanner(), new Line(LANG_SHELL, BLANK),   "\n");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, BLANK),   "     \n");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, BLANK),   "\t\n");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, CODE),    "echo \"hello\"\n");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, COMMENT), "# Line comment\n");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, CODE),    "ls # with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new ShellScanner(), new Line(LANG_SHELL, BLANK),   "     ");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, BLANK),   "\t");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, CODE),    "echo \"hello\"");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, COMMENT), "# Line comment");
		assertLine(new ShellScanner(), new Line(LANG_SHELL, CODE),    "ls # with comment");
	}

	@Test
	public void simpleTest() {
		String code
			= "#!/bin/bash\n"
			+ "\t\n"
			+ "# print the name and contents of the current\n"
			+ "# working directory\n"
			+ "echo \"This is from a shell script:\"\n"
			+ "pwd     # print the directory name\n"
			+ "ls      # print the directory contents\n";

		Line[] expected = {
			new Line(LANG_SHELL, COMMENT),
			new Line(LANG_SHELL, BLANK),
			new Line(LANG_SHELL, COMMENT),
			new Line(LANG_SHELL, COMMENT),
			new Line(LANG_SHELL, CODE),
			new Line(LANG_SHELL, CODE),
			new Line(LANG_SHELL, CODE)
		};
		assertLines(new ShellScanner(), expected, code);
	}

	@Test
	public void unterminatedMultilineStringCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "'\nA\n\n";

		Line[] expected = {
				new Line(LANG_SHELL, CODE),
				new Line(LANG_SHELL, CODE),
				new Line(LANG_SHELL, BLANK)
			};
		assertLines(new ShellScanner(), expected, code);
	}
	
}
