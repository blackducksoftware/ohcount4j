package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class PrologScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "     \n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\t\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!').\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "/* Block Comment */\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "% Line comment\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "%\n");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!'). // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "     ");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, BLANK), "\t");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!').");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "/* Block Comment */");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "% Line comment");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, COMMENT), "%");
        assertLine(Language.PROLOG, new Line(Language.PROLOG, CODE), "hello_world :- write('Hello World!'). // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "/* QuickSort\n"
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
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, BLANK),
                new Line(Language.PROLOG, COMMENT),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, BLANK),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE),
                new Line(Language.PROLOG, CODE)
        };
        assertLines(Language.PROLOG, expected, code);
    }

}
