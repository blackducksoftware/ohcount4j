package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class SmalltalkScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, BLANK),   "\n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, BLANK),   "     \n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, BLANK),   "\t\n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, CODE),    "^Student new name: aPerson name\n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"\n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, COMMENT), "\"\"\n");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, CODE),    " y := y + 7. // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, BLANK),   "     ");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, BLANK),   "\t");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, CODE),    "^Student new name: aPerson name");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, COMMENT), "\"\"");
		assertLine(new SmalltalkScanner(), new Line(Language.SMALLTALK, CODE),    " y := y + 7. // with comment");
	}

	@Test
	public void sampleTest() {
		String code
			= "\"Simple piece of a Smalltalk\n"
			+ "			Program\n"
			+ "\n"
			+ "\"\n"
			+ "name: aName address: adAddress\n"
			+ "\"Set the receiver's name and address\n"
			+ "to the specified values.\"\n"
			+ "self name: aName.\n"
			+ "self address: anAddress\n";

		Line[] expected = {
			new Line(Language.SMALLTALK, COMMENT),
			new Line(Language.SMALLTALK, COMMENT),
			new Line(Language.SMALLTALK, BLANK),
			new Line(Language.SMALLTALK, COMMENT),
			new Line(Language.SMALLTALK, CODE),
			new Line(Language.SMALLTALK, COMMENT),
			new Line(Language.SMALLTALK, COMMENT),
			new Line(Language.SMALLTALK, CODE),
			new Line(Language.SMALLTALK, CODE)
		};
		assertLines(new SmalltalkScanner(), expected, code);
	}

	@Test
	public void unterminatedBlockCommentCrash() {
		// This minimal case caused an Arrays.copyOfRange() crash
		String code = "\"\n\n\n";

		Line[] expected = {
				new Line(Language.SMALLTALK, COMMENT),
				new Line(Language.SMALLTALK, BLANK),
				new Line(Language.SMALLTALK, BLANK)
			};
		assertLines(new SmalltalkScanner(), expected, code);
	}
}
