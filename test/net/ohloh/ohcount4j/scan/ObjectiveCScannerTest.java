package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import static net.ohloh.ohcount4j.Language.*;

public class ObjectiveCScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, BLANK),   "\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, BLANK),   "     \n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, BLANK),   "\t\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, CODE),    "NSMutableArray *myArray = nil;\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "/* Block Comment */\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "// Line comment\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "//\n");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, CODE),    "NSMutableArray *myArray = nil;  // nil is essentially the same as NULL\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, BLANK),   "     ");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, BLANK),   "\t");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, CODE),    "NSMutableArray *myArray = nil;");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "/* Block Comment */");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "// Line comment");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, COMMENT), "//");
		assertLine(new ObjectiveCScanner(), new Line(LANG_OBJECTIVEC, CODE),    "NSMutableArray *myArray = nil;  // nil is essentially the same as NULL");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Objective C \"Hello World\"\n"
			+ "Test Program\n"
			+ "\n"
			+ "*/\n"
			+ "#import <Foundation/Foundation.h>\n"
			+ "\n"	 
			+ "int main (int argc, const char * argv[])\n"
			+ "{\n"
			+ "		NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];\n"
			+ "	    NSLog (@\"Hello, World!\");\n"
			+ "	    [pool drain];\n"
			+ "	    return 0;\n"
			+ "}\n"
			+ "//";

		Line[] expected = {
			new Line(LANG_OBJECTIVEC, COMMENT),
			new Line(LANG_OBJECTIVEC, COMMENT),
			new Line(LANG_OBJECTIVEC, BLANK),
			new Line(LANG_OBJECTIVEC, COMMENT),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, BLANK),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, CODE),
			new Line(LANG_OBJECTIVEC, COMMENT)
		};
		assertLines(new ObjectiveCScanner(), expected, code);
	}

}