#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    int jump(vector<int>& nums) {
        int nJumps = 0;
        const int n = nums.size();
        if (n == 1) { return 0; }
        int pos = 0;
        
        while (true) {
            const int value = nums[pos];
            if (pos+value >= n-1) { return nJumps + 1; }

            int chosen = 0;
            int range = 0;
            for (int i=1; i<=value; i++) {
                const int newRange = i + nums[pos+i];
                if (newRange > range) {
                    range = newRange;
                    chosen = i;
                }
            }
            pos = pos + chosen;
            nJumps++;
        }
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {2,3,1,1,4};

    int result = solution.jump(nums);

    cout << result << endl;
    return 0;
}