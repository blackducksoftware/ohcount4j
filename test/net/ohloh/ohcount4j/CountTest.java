package net.ohloh.ohcount4j;

import org.testng.annotations.Test;
import static org.testng.AssertJUnit.*;

import net.ohloh.ohcount4j.Count;
import static net.ohloh.ohcount4j.Language.*;

public class CountTest {

	@Test
	public void compareTo() {
		// Sort by code lines first
		assertEquals( 0, new Count(LANG_C, 10, 0, 0).compareTo(new Count(LANG_C, 10, 0, 0)));
		assertEquals( 1, new Count(LANG_C, 11, 0, 0).compareTo(new Count(LANG_C, 10, 0, 0)));
		assertEquals(-1, new Count(LANG_C,  9, 0, 0).compareTo(new Count(LANG_C, 10, 0, 0)));

		// When code lines are equal, sort by comment lines
		assertEquals( 0, new Count(LANG_C, 10, 10, 0).compareTo(new Count(LANG_C, 10, 10, 0)));
		assertEquals( 1, new Count(LANG_C, 10, 11, 0).compareTo(new Count(LANG_C, 10, 10, 0)));
		assertEquals(-1, new Count(LANG_C, 10,  9, 0).compareTo(new Count(LANG_C, 10, 10, 0)));

		// When code and comment lines are equal, sort by blank lines
		assertEquals( 0, new Count(LANG_C, 10, 10, 10).compareTo(new Count(LANG_C, 10, 10, 10)));
		assertEquals( 1, new Count(LANG_C, 10, 10, 11).compareTo(new Count(LANG_C, 10, 10, 10)));
		assertEquals(-1, new Count(LANG_C, 10, 10,  9).compareTo(new Count(LANG_C, 10, 10, 10)));
	}

	@Test
	public void equals() {
		assertEquals(new Count(LANG_C, 1, 2, 3), new Count(LANG_C, 1, 2, 3));

		// No assertNotEqual() method? Sigh....
		assertFalse(new Count(null,   1, 2, 3).equals(new Count(LANG_C, 1, 2, 3)));
		assertFalse(new Count(LANG_C, 0, 2, 3).equals(new Count(LANG_C, 1, 2, 3)));
		assertFalse(new Count(LANG_C, 1, 0, 3).equals(new Count(LANG_C, 1, 2, 3)));
		assertFalse(new Count(LANG_C, 1, 2, 0).equals(new Count(LANG_C, 1, 2, 3)));
	}

	@Test
	public void add() {
		assertEquals(
				new Count(LANG_C, 33, 55, 77),
				new Count(LANG_C, 3, 5, 7).add(new Count(LANG_C, 30, 50, 70))
				);
	}
}