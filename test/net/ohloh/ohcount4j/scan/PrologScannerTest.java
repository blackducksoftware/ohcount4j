package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.LANG_PROLOG;

public class PrologScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, BLANK),   "\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, BLANK),   "     \n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, BLANK),   "\t\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, CODE),    "hello_world :- write('Hello World!').\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "/* Block Comment */\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "% Line comment\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "%\n");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, CODE),    "hello_world :- write('Hello World!'). // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, BLANK),   "     ");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, BLANK),   "\t");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, CODE),    "hello_world :- write('Hello World!').");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "/* Block Comment */");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "% Line comment");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, COMMENT), "%");
		assertLine(new PrologScanner(), new Line(LANG_PROLOG, CODE),    "hello_world :- write('Hello World!'). // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "/* QuickSort\n"
			+ " * Written in Prolog\n"
			+ "\n"
			+ " */\n"
			+ "partition([], _, [], []).\n"
			+ "partition([X|Xs], Pivot, Smalls, Bigs) :-\n"
			+ "		(   X @< Pivot ->\n"
			+ "			Smalls = [X|Rest],\n"
			+ "			partition(Xs, Pivot, Rest, Bigs)\n"
			+ "		;   Bigs = [X|Rest],\n"
			+ "			partition(Xs, Pivot, Smalls, Rest)\n"
			+ "		).\n"
			+ "\t\n"
			+ "quicksort([])     --> [].\n"
			+ "quicksort([X|Xs]) -->\n"
			+ "		{ partition(Xs, X, Smaller, Bigger) },\n"
			+ "		quicksort(Smaller), [X], quicksort(Bigger).\n";

		Line[] expected = {
			new Line(LANG_PROLOG, COMMENT),
			new Line(LANG_PROLOG, COMMENT),
			new Line(LANG_PROLOG, BLANK),
			new Line(LANG_PROLOG, COMMENT),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, BLANK),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE),
			new Line(LANG_PROLOG, CODE)
		};
		assertLines(new PrologScanner(), expected, code);
	}

}