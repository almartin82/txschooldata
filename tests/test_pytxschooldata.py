"""
Tests for pytxschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pytxschooldata
    assert pytxschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pytxschooldata
    assert hasattr(pytxschooldata, 'fetch_enr')
    assert callable(pytxschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pytxschooldata
    assert hasattr(pytxschooldata, 'get_available_years')
    assert callable(pytxschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pytxschooldata
    assert hasattr(pytxschooldata, '__version__')
    assert isinstance(pytxschooldata.__version__, str)
