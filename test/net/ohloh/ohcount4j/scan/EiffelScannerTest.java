package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class EiffelScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, BLANK),   "\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, BLANK),   "     \n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, BLANK),   "\t\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, CODE),    "number := phone_book [\"JILL SMITH\"]\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, COMMENT), "-- Line comment\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, COMMENT), "--\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, CODE),    "number := phone_book [\"JILL SMITH\"] -- with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, BLANK),   "     ");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, BLANK),   "\t\n");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, CODE),    "number := phone_book [\"JILL SMITH\"]");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, COMMENT), "-- Line comment");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, COMMENT), "--");
		assertLine(new EiffelScanner(), new Line(Language.EIFFEL, CODE),    "number := phone_book [\"JILL SMITH\"] -- with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "-- Hello World Eiffel Program\n"
			+ "class HELLO_WORLD\n"
			+ "\n"
			+ "creation\n"
			+ "		make\n"
			+ "feature\n"
			+ "		make is\n"
			+ "		local\n"
            + "    		io:BASIC_IO\n"
            + "		do\n"
            + "		    !!io\n"
            + "			io.put_string(\"%N Hello World!!!!\")\n"
            + "		end --make\n"
            + "\n"
            + "end -- class HELLO_WORLD";

		Line[] expected = {
			new Line(Language.EIFFEL, COMMENT),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, BLANK),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, CODE),
			new Line(Language.EIFFEL, BLANK),
			new Line(Language.EIFFEL, CODE)
		};
		assertLines(new EiffelScanner(), expected, code);
	}

}
