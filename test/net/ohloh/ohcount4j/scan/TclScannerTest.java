package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class TclScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new TclScanner(), new Line(Language.TCL, BLANK),   "\n");
		assertLine(new TclScanner(), new Line(Language.TCL, BLANK),   "     \n");
		assertLine(new TclScanner(), new Line(Language.TCL, BLANK),   "\t\n");
		assertLine(new TclScanner(), new Line(Language.TCL, CODE),    "puts $var($index)\n");
		assertLine(new TclScanner(), new Line(Language.TCL, COMMENT), "# Line comment\n");
		assertLine(new TclScanner(), new Line(Language.TCL, COMMENT), "#\n");
		assertLine(new TclScanner(), new Line(Language.TCL, CODE),    "puts $var($index) # with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new TclScanner(), new Line(Language.TCL, BLANK),   "     ");
		assertLine(new TclScanner(), new Line(Language.TCL, BLANK),   "\t");
		assertLine(new TclScanner(), new Line(Language.TCL, CODE),    "puts $var($index)");
		assertLine(new TclScanner(), new Line(Language.TCL, COMMENT), "# Line comment");
		assertLine(new TclScanner(), new Line(Language.TCL, COMMENT), "#");
		assertLine(new TclScanner(), new Line(Language.TCL, CODE),    "puts $var($index) # with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "@x = (1, 2, 3);      # Create a 3 element list/array\n"
			+ "\t\n"
			+ "foreach $i (@x) {\n"
		    + "    print $i++ . \"-\";\n"
		 	+ "}\n"
		 	+ "# At this point, array contains 2, 3, 4!\n";

		Line[] expected = {
			new Line(Language.TCL, CODE),
			new Line(Language.TCL, BLANK),
			new Line(Language.TCL, CODE),
			new Line(Language.TCL, CODE),
			new Line(Language.TCL, CODE),
			new Line(Language.TCL, COMMENT)
		};
		assertLines(new TclScanner(), expected, code);
	}

}
