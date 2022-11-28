using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WindIndicator : MonoBehaviour
{
    [SerializeField] private WindDataScriptableObject windDataSO;
    
    // Update is called once per frame
    void Update()
    {
        transform.forward = windDataSO.WindDirection;
    }
}
