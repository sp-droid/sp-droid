#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
public:
    int maxProfit(vector<int>& prices) {
        int totalProfit = 0;
        int n = prices.size();
        if (n<2) { return 0; }

        int buy = prices[0];
        int sell = 0;

        for (int i=1; i<n; i++) {
            sell = prices[i];
            const int profit = sell-buy;

            if (profit>0) {
                totalProfit += profit;
            }
            buy = sell;
        }

        return totalProfit;
    }
};

int main() {
    Solution solution;
    
    vector<int> prices = {7,1,5,3,6,4};

    int profit = solution.maxProfit(prices);

    cout << profit << endl;
    return 0;
}