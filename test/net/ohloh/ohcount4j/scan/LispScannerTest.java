package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_LISP;

public class LispScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new LispScanner(), new Line(LANG_LISP, BLANK),   "\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, BLANK),   "     \n");
		assertLine(new LispScanner(), new Line(LANG_LISP, BLANK),   "\t\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, CODE),    "((lambda (arg) (+ arg 1)) 5)\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), "#| Block Comment |#\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), "; Line comment\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), ";\n");
		assertLine(new LispScanner(), new Line(LANG_LISP, CODE),    "((lambda (arg) (+ arg 1)) 5) ; with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new LispScanner(), new Line(LANG_LISP, BLANK),   "     ");
		assertLine(new LispScanner(), new Line(LANG_LISP, BLANK),   "\t");
		assertLine(new LispScanner(), new Line(LANG_LISP, CODE),    "((lambda (arg) (+ arg 1)) 5)");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), "#| Block Comment |#");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), "; Line comment");
		assertLine(new LispScanner(), new Line(LANG_LISP, COMMENT), ";");
		assertLine(new LispScanner(), new Line(LANG_LISP, CODE),    "((lambda (arg) (+ arg 1)) 5) ; with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= ";;; This function simply returns the string Hello World that is in quotes.\n"
			+ "#| Rarely used but, \n"
			+ "   \n"
			+ "   multi-line comments can exist in this form |#\n"
			+ "\t\n"
			+ "(DEFUN HELLO ()\n"
			+ "		\"Hello world\n"
			+ "					documentation string\"\n"
			+ "		\"MULTI-LINE~\n"
			+ "		;STRING WITH SEMI-COLON\"\n"
			+ ")\n"
			+ ";"; 

		Line[] expected = {
			new Line(LANG_LISP, COMMENT),
			new Line(LANG_LISP, COMMENT),
			new Line(LANG_LISP, BLANK),
			new Line(LANG_LISP, COMMENT),
			new Line(LANG_LISP, BLANK),
			new Line(LANG_LISP, CODE),
			new Line(LANG_LISP, COMMENT),
			new Line(LANG_LISP, COMMENT),
			new Line(LANG_LISP, CODE),
			new Line(LANG_LISP, CODE),
			new Line(LANG_LISP, CODE),
			new Line(LANG_LISP, COMMENT)
		};
		assertLines(new LispScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "#|\n\n\n";

		Line[] expected = {
				new Line(LANG_LISP, COMMENT),
				new Line(LANG_LISP, BLANK),
				new Line(LANG_LISP, BLANK)
			};
		assertLines(new LispScanner(), expected, code);
	}
	
}