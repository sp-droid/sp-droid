#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    int majorityElement(vector<int>& nums) {
        std::unordered_map<int, int> counts;

        for (int num : nums) {
            if (counts.find(num) == counts.end()) { // Number not found in the hashmap
                counts[num] = 1; // Entry for the number
            } else {
                counts[num]++;
            }
        }

        int maxKey = -1;
        int maxValue = 0;
        for (const auto& pair : counts) {
            if (pair.second > maxValue) {
                maxKey = pair.first;
                maxValue = pair.second;
            }
        }

        return maxKey;
    }
};

int main() {
    Solution solution;
    
    vector<int> nums = {3,2,3};

    int output = solution.majorityElement(nums);

    cout << output << endl;
    return 0;
}