import importlib.util
import pathlib
import unittest

spec = importlib.util.spec_from_file_location(
    'r4_result', pathlib.Path(__file__).resolve().parents[2] / 'scripts/r4/result.py')
r4_result = importlib.util.module_from_spec(spec)
spec.loader.exec_module(r4_result)


class HourVerdict(unittest.TestCase):
    def setUp(self):
        self.node = {'liveSeconds': 3600, 'liveBlocks': 7, 'executed': 16,
                     'certified': 16, 'head': 120, 'anchor': 104}
        self.samples = [dict(monotonic=t, canonical=True, state_ok=True, lag=3, head=110)
                        for t in range(0, 3601, 5)]
        self.samples[0]['lag'] = 0

    def verdict(self):
        return r4_result.grade(self.node, self.samples)['score']

    def test_final_lag_of_three_is_not_a_failure(self):
        self.assertEqual(self.verdict(), 'yes')

    def test_noncanonical_probe_is_a_failure(self):
        self.samples[5]['canonical'] = False
        self.assertEqual(self.verdict(), 'no')

    def test_excess_lag_is_a_failure(self):
        self.samples[5]['lag'] = 65
        self.assertEqual(self.verdict(), 'no')

    def test_excess_gap_is_a_failure(self):
        del self.samples[10:15]
        self.assertEqual(self.verdict(), 'no')

    def test_short_window_is_a_failure(self):
        self.samples.pop()
        self.assertEqual(self.verdict(), 'no')

    def test_missing_certificate_is_a_failure(self):
        self.node['certified'] -= 1
        self.assertEqual(self.verdict(), 'no')


if __name__ == '__main__':
    unittest.main()
