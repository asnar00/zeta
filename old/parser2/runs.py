 def find_runs(self, ms: List[Match]) -> List[Run]:
        i_match = 0
        all_runs = []
        while i_match < len(ms):
            runs, longest_run_length = self.find_runs_from(ms, i_match)
            all_runs += runs
            if longest_run_length == 0: break
            i_match += longest_run_length
        all_runs.sort(key = lambda run: run.start, reverse=True)
        log("all_runs", all_runs)
        return all_runs
    
    def find_runs_from(self, ms: List[Match], i_match: int) -> List[Run]:
        runs = self.init_runs(ms, i_match)
        longest_run_length = max([(run.length()) if run else 0 for run in runs])
        longest = [run for run in runs if run and run.length() == longest_run_length]
        # todo: apply "overshadowing" rules
        longest = self.remove_overshadowed(longest)
        # return only the completed runs, but also return the length so we can step past
        longest_completed = [run for run in longest if run.is_complete()]
        return longest, longest_run_length
    
    def init_runs(self, ms: List[Match], i_match:int) -> List[Run]:
        match = ms[i_match]
        n_rules = len(match.bitmask_list)
        runs = []
        for i_rule in range(n_rules):
            if match.bitmask_list[i_rule] > 0:
                run = Run(s_mg.rules[i_rule], i_match, i_match)
                run = self.extend_run(run, i_match, ms)
                runs.append(run)
            else: runs.append(None)
        return runs





#------------------------------------------------------------------------
    def parse_step(self, ms: List[Match]) -> List[Match]:
        runs = self.find_runs(ms)
        self.show(ms, runs)
        for run in runs:
            if run.is_complete():
                rule_name = run.rule.name
                log("collapse", rule_name, run.start, run.end)
                ms = self.collapse(ms, run.start, run.end, rule_name)
        return ms
    
    def find_runs(self, ms: List[Match]) -> List[Run]:
        n_rules = len(s_mg.rules)
        runs = []
        for i_rule in range(n_rules):
            runs += self.scan_runs(i_rule, ms)
        runs = self.filter_runs(runs)
        runs.sort(key = lambda run: run.start, reverse=True)
        log("runs:", runs)
        return runs
    
    def scan_runs(self, i_rule: int, ms: List[Match]) -> List[Run]:
        runs = []
        i_match = 0
        while i_match < len(ms):
            # scan forward until bitmask is non-zero
            while i_match < len(ms) and ms[i_match].bitmask_list[i_rule] == 0: i_match += 1
            if i_match >= len(ms): break
            # start a run
            run = Run(s_mg.rules[i_rule], i_match, i_match-1)
            run = self.extend_run(run, i_match, ms)
            if run.length() > 0: 
                runs.append(run)
                i_match = run.end + 1
            else:
                i_match += 1
        return runs
    
    def filter_runs(self, runs: List[Run]) -> List[Run]:
        runs = self.remove_overshadowed(runs)
        return runs
    
    def extend_run(self, run: Run, i_match: int, ms: List[Match]) -> Run:
        while i_match < len(ms):
            if run.is_complete(): return run
            run, matched = self.match_run(run, ms[i_match].bitmask_list[run.i_rule()])
            if matched: run.end = i_match
            else: return run
            i_match += 1
        return run
    
    # returns the updated run, and True/False
    def match_run(self, run: Run, bitmask: int) -> Tuple[Run, bool]:
        vb = run.rule.name == "brackets"
        rule = run.rule
        matches_i_term = self.bitmask_contains(bitmask, run.i_term)
        if matches_i_term:
            if vb: log("matches_i_term")
            if not isinstance(rule.terms[run.i_term], ZeroOrMore):
                run.i_term += 1 # don't step to the next expected term if we're matching a list
            return run, True
        else: # term (i_term) might be optional, in which case try against (i_term+1)
            if (run.i_term + 1) < len(rule.terms) and rule.terms[run.i_term].is_optional():
                if vb: log("matches i_term+1")
                matches_next_term = self.bitmask_contains(bitmask, run.i_term + 1)
                if matches_next_term:
                    if isinstance(rule.terms[run.i_term], ZeroOrMore): run.i_term += 1
                    else: run.i_term += 2
                    return run, True
        if vb: log("no match")
        return run, False
            
    def remove_overshadowed(self, runs: List[Run]) -> List[Run]:
        log("remove_overshadowd", runs)
        i_remove = []
        for i in range(0, len(runs)):
            for j in range(0, len(runs)):
                if i == j: continue
                i_rule = runs[i].i_rule()
                j_rule = runs[j].i_rule()
                if s_mg.is_overshadowed_by[j_rule] == i_rule:
                    i_remove.append(j)
        return [runs[i] for i in range(0, len(runs)) if not i in i_remove]