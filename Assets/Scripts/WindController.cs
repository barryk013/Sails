using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class WindController : MonoBehaviour
{
    [SerializeField] private WindDataScriptableObject windDataSO;
    [SerializeField] private Slider angleSlider;

    private void Awake()
    {
        angleSlider.onValueChanged.AddListener(OnAngleChanged);
    }

   
    private void OnAngleChanged(float angle)
    {
        windDataSO.Angle = angle;
    }
}
